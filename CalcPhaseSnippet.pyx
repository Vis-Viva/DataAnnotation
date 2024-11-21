	### CALC PHASE OPS ###

	"""
	NOTATION: 
		𝓦(x,y) = ConnectionGraph weights from x to y, shape (1326,T)
		Pl(x) = Acting player @ x
		𝓟(x) = GT path to x, 𝓟(x)y+ = direct successor to y in 𝓟(x)
		𝓹(x) = GTParent(x), POV𝓹(x) = first node n above x:Pl(n)=POV
		πₒ(x) = Opp-only counterfactual reach prob for x, shape (1326,T)
		π(x,y) = fwd reach prob from x to y, shape (1326,T)
		𝓝ₑ = Set of all fully-explored nodes
		𝓝ₛ = Set of all solvable nodes (⊆ 𝓝ₑ)
		Solvable node := node n∈𝓝ₑ: s∈𝓝ₑ ∀ n∈S(n)
		Sᵣ = Root of subgame S
		𝓢ₛ = Set of all solvable subgames (partitions 𝓝ₛ)
		Sₛ = Solvable subgame ∈ 𝓢ₛ
		Solvable subgame := Subgame S:[(POV𝓹(Sᵣ)∉𝓝ₛ) & (n∈𝓝ₛ ∀n∈S)]

	ARR AXES:
		N.ConnectionGraph: axes=(aIdx, handIdx, tIdx) aka (ACTIONS, HISTORIES, ITERS), shape=(|N.A|,1326,T)
			if N.ActingPlayer=POV: CGraph[a,h₁,t] = CGraph[a,h₂,t] since h₁=h₂ ∀h₁,h₂ 
			if N.ActingPlayer=opp: CGraph[a,h,t] = σᵗₒ(Iₒ(h),a) wherever h=idx of a possible opp hand, 0 elsewhere
		N.CFReaches:  axes=(oppHandIdx, tIdx) aka (HISTORIES, ITERS), shape=(1326,T)
		N.FwdReaches: axes=(oppHandIdx, tIdx) aka (HISTORIES, ITERS), shape=(1326,T)
			N.[CF/Fwd]Reaches[ h,t ] = reaches derived using σᵗₒ(Iₒ(h)) wherever opp probabilities are required
			As above, these will be 0 wherever h is not an index of a possible opp hand
	"""

	#Step up GT path from currentNode until a POVplayer node is found, then return its key 
	cdef ll PrecedingPNodeKey( CFRCollector self, ll nKey ):

		cdef GTNode currentNode = self.at( nKey )
		if currentNode.GTParentKey==0: return 0

		cdef GTNode prevNode = self.at( currentNode.GTParentKey )
		while prevNode.ActingPlayer != self.POVplayer:
			if prevNode.GTParentKey==0: return 0
			prevNode = self.at( prevNode.GTParentKey )

		return prevNode.Key

	cdef bint All_SubNodes_Explored( CFRCollector self, ll nKey ):
		cdef:
			vector_ll S = self.at( nKey ).SubKeys
			uint      s
			GTNode    subNode
		for s from 1 <= s <= S.size:
			subNode = self.at( S.at( s-1 ) )
			if (subNode.ActingPlayer==self.POVplayer) and (subNode.Fully_Explored==0): return 0
		return 1

	cdef void find_solvable_nodes( CFRCollector self ):

		print( f"\n\tFinding all solvable nodes in set of {self.FullyExploredKeys.size} fully explored nodes..." )

		cdef uint k
		cdef ll   expKey
		for k from 1 <= k <= self.FullyExploredKeys.size:
			expKey  = self.FullyExploredKeys.at( k-1 )
			if self.All_SubNodes_Explored( expKey ):
				self.at( expKey ).Solvable = 1
				self.SolvableKeys.append( expKey )

		print( f"\t{self.SolvableKeys.size} solvable nodes found, proceeding to solvable subgame search..." )

	cdef void find_solvable_subgames( CFRCollector self ):

		print( f"\n\tFinding solvable Sᵣ in set of {self.SolvableKeys.size} solvable nodes..." )

		cdef uint k
		cdef ll   rootKey, parentKey
		for k from 1 <= k <= self.SolvableKeys.size:
			rootKey   = self.SolvableKeys.at( k-1 )
			parentKey = self.PrecedingPNodeKey( rootKey )
			if (parentKey==0) or (self.at( parentKey ).Solvable==0): self.SolvableSubgames.append( rootKey )

		print( f"\t{self.SolvableSubgames.size} solvable subgames found, proceeding to set counterfactual payoffs..." )

	#Find 𝓝ₛ⊆𝓝ₑ ⟶ Find 𝓢ₛ⊆𝓝ₛ ⟶ set CFU(z) ∀ z∈Z[Sₛ] ∀ Sₛ∈𝓢ₛ ⟶ init n.ZMap ∀ n∈𝓝ₛ ⟶ path-prop CFU(z) ∀ z∈Z[Sₛ] ∀ Sₛ∈𝓢ₛ
	#Basically, finds all solvable paths and transmits along those paths the endgame data we need for calculating advs
	cdef void build_solvable_paths( CFRCollector self ):

		print( '\n'+("="*50) )		
		print( f"FINDING SOLVABLE PATHS".center(50) )
		print( "="*50 )

		cdef double pathStart = TimeNow()
		self.find_solvable_nodes()
		self.find_solvable_subgames()
		#self.set_solvable_cfpayoffs() #DEPRECATED
		#self.initialize_solvable_zmaps() #DEPRECATED
		#self.propagate_solvable_cfpayoffs() #DEPRECATED
		cdef double pathTime = TimeNow()-pathStart
		cdef uint K = self.SegmentTravsDone

		print( '\n'+("="*50) )		
		print( f"ALL SOLVABLE PATHS FOUND".center(50) )
		print( "="*50 )
		print( f"Time taken: {pathTime:.3f}sec" )
		print( f"|𝓢ₛ| = {self.SolvableSubgames.size}" )
		print( f"|𝓝ₛ| = {self.SolvableKeys.size}" )

	cdef void initialize_fwd_reach_maps( CFRCollector self ):

		print(f"\nInitializing πfwd maps ∀ n∈Sₛ ∀ Sₛ∈𝓢ₛ...")

		cdef:
			uint      s, ss, nSS = self.SolvableSubgames.size, nNodes=0, nInit=0
			ll        Sr, subKey
			vector_ll S
			double    initStart = TimeNow()

		for s from 1 <= s <= nSS:
			Sr = self.SolvableSubgames.at( s-1 )
			S  = self.at( Sr ).SubKeys
			nNodes += (S.size+1) #+1 because SubKeys doesn't include Sᵣ itself

		for s from 1 <= s <= nSS:
			Sr = self.SolvableSubgames.at( s-1 )
			S  = self.at( Sr ).SubKeys
			self.at( Sr ).initialize_fwd_reaches()
			for ss from 1 <= ss <= S.size:
				subKey = S.at( ss-1 )
				self.at( subKey ).initialize_fwd_reaches(); nInit+=1
				print( PB( nInit, nNodes ) + f" {nInit}/{nNodes}", end='\r' )

		cdef double initTime = TimeNow()-initStart
		print( f"\n{nNodes} πfwd maps successfully initialized, time taken: {initTime:.3f}sec" )

	cdef GTNode GTParent( CFRCollector self, ll of_key ):
		return self.at( self.at( of_key ).GTParentKey )

	#∀z∈𝓩, π(𝓹(z),z) = 𝓦(𝓹(z),z), & since 𝓦 is calculated upon init, ∃ 𝓦(𝓹(z),z) ∀ z∈𝓩 already
	cdef void set_base_fwd_reaches( CFRCollector self ):

		print(f"\nCalculating π(𝓹(z),z) ∀ z∈𝓩...")

		cdef:
			uint   z, nZ = self.zKeys.size
			ll     zKey
			double zTime, zStart = TimeNow()
			flt2   zReach
			GTNode pz

		for z from 1 <= z <= nZ:
			zKey   = self.zKeys.at( z-1 )
			pz     = self.GTParent( of_key=zKey )
			zReach = pz.ConnectionGraph.get_weights( to_key=zKey ) #(1326,T)

			if pz.FwdReaches is None: pz.initialize_fwd_reaches() #if pz.ActingPlayer = opp
			pz.FwdReaches.set_weights( to_key=zKey, weights=zReach )
			print( PB( z,nZ ) + f" {z}/{nZ}", end='\r' )

		zTime = TimeNow()-zStart
		print( f"\nπ(𝓹(z),z) calculated ∀ z∈𝓩, time taken: {zTime:.3f}sec" )

	cdef uint __count_steps( CFRCollector self, uint by_player, vector_ll along_path ):
		cdef uint s, pSteps=0, pathLen = along_path.size
		cdef ll   stepKey
		for s from 1 <= s <= pathLen:
			stepKey = along_path.at( s-1 )
			if self.at( stepKey ).ActingPlayer==by_player: pSteps+=1
		return pSteps

	#πₒ(Sᵣ) = Π( 𝓦(o,n) ) ∀ o,n∈𝓟(S):(Pl(o)=clown & n=𝓟(S)o+)
	cdef matrix_flt RootCFReach( CFRCollector self, ll Sr ):

		cdef:
			vector_ll PS          = self.at( Sr ).PathKeys()
			uint      clown       = OpponentsOf( self.POVplayer )[ 0 ], pathLen = PS.size,                             \
					  oSteps      = self.__count_steps( by_player=clown, along_path=PS ),                              \
					  PATHSTEPS   = 0, nH = NUM_POSSIBLE_HANDS, oppStep = 1, step
			flt3      pathWeights = cyarr( (oSteps+1, nH, T), FLTSIZE, 'f' )
			ll        stepKey, nextKey

		pathWeights[0,:,:] = 1
		for step from 0 <= step < pathLen-1:
			stepKey = PS.at( step )
			if self.at( stepKey ).ActingPlayer==clown:
				nextKey = PS.at( step+1 )
				pathWeights[ oppStep,:,: ] = self.at( stepKey ).ConnectionGraph.get_weights( to_key=nextKey ) #(1326,T)
				oppStep+=1

		cdef flt2 rootCFReaches = NP( pathWeights ).prod( axis=PATHSTEPS ) #(1326,T)
		return matrix_flt( from_view=rootCFReaches ) #(1326,T)

	#To calculate πₒ(n) ∀ n∈Sₛ, first need πₒ(Sᵣ) as inductive base. ∴ start by calculating πₒ(Sᵣ) ∀ Sₛ∈𝓢ₛ
	cdef void calculate_base_cfreaches( CFRCollector self ):

		cdef:
			uint   s, nSS = self.SolvableSubgames.size
			ll     Sr
			double cfStart = TimeNow(), cfTime

		print( f"\nCalculating πₒ(Sᵣ) ∀ S∈𝓢ₛ..." )

		for s from 1 <= s <= nSS:
			Sr = self.SolvableSubgames.at( s-1 )
			self.at( Sr ).CFReaches = self.RootCFReach( Sr ) #(1326,T)
			print( PB( s,nSS ) + f" {s}/{nSS}", end='\r' )
		cfTime = TimeNow()-cfStart

		print( f"\nπₒ(Sᵣ) ∀ S∈𝓢ₛ successfully determined, time taken: {cfTime:.3f}sec" )

	#πₒ skips POV nodes, ∴ Pl(n)=POV ⇒ (πₒ(n)=πₒ( 𝓹(n) )), & Pl(n)=opp ⇒ πₒ(n)=πₒ( 𝓹(n) )*𝓦( 𝓹(n),n )
	cdef matrix_flt CFReach( CFRCollector self, ll nKey ):
		cdef:
			flt2   leadingWeights, cfview
			uint   clown      = OpponentsOf( self.POVplayer )[ 0 ]
			GTNode parentNode = self.GTParent( nKey ) 
		if parentNode.ActingPlayer == self.POVplayer: return parentNode.CFReaches #(1326,T)
		if parentNode.ActingPlayer == clown:
			leadingWeights = parentNode.ConnectionGraph.get_weights( to_key=nKey ) #(1326,T)
			cfview         = ArrMult2d( parentNode.CFReaches.view(), leadingWeights ) #(1326,T)
			return matrix_flt( from_view=cfview ) #(1326,T)

	cdef void accumulate_zpath_cfreaches( CFRCollector self, ll Sr, ll zKey ):
		cdef:
			vector_ll Pz   = self.at( zKey ).TerminalPath #𝓟(z)
			uint      rIdx = Pz.index_of( Sr ), pathStep
			ll1       SPz  = Pz.view()[ rIdx: ] #𝓟(z)∩S, i.e. just the part of 𝓟(z) which lies inside of S
			ll        stepKey
		for pathStep from 1 <= pathStep <= SPz.shape[ 0 ]:
			stepKey  = SPz[ pathStep-1 ]
			if self.at( stepKey ).CFReaches is None: #Only calc πₒ(n) if not done already for an overlapping zpath
				self.at( stepKey ).CFReaches = self.CFReach( stepKey ) #(1326,T)

	#Once a solvable subgame's root cfreach is calculated, we can inductively calculate all cfreaches within the subgame
	cdef void calculate_subgame_cfreaches( CFRCollector self, ll Sr ):
		cdef:
			vector_ll ZS = self.at( Sr ).Zn #Z[S]
			ll        zKey
			uint      z
		for z from 1 <= z <= ZS.size:
			zKey = ZS.at( z-1 )
			self.accumulate_zpath_cfreaches( Sr,zKey )

	cdef void calculate_counterfactual_reaches( CFRCollector self ):

		cdef:
			uint   s, nSS = self.SolvableSubgames.size
			ll     Sr
			double cfStart = TimeNow(), cfTime

		print( f"\nCalculating πₒ(n) ∀ n∈Sₛ ∀ Sₛ∈𝓢ₛ..." )

		for s from 1 <= s <= nSS:
			Sr = self.SolvableSubgames.at( s-1 )
			self.calculate_subgame_cfreaches( Sr )
			print( PB( s,nSS ) + f" {s}/{nSS}",end='\r' )

		cfTime = TimeNow() - cfStart
		print( f"\nπₒ ∀ Sₛ∈𝓢ₛ successfully determined, time taken: {cfTime:.3f}sec" )

	#vKey=𝓟(z)fKey+ ⇒ ∃π(vKey,z) already; ∴π(fKey,z) = 𝓦(fKey,vKey) * π(vKey,z), since we're back-stepping through 𝓟(z)
	cdef flt2 TerminalFwdReach( CFRCollector self, ll from_key, ll via_key, ll to_zkey ):
		cdef flt2 hereToNext = self.at( from_key ).ConnectionGraph.get_weights( to_key=via_key ),                      \
				  nextToEnd  = self.at( via_key ).FwdReaches.get_weights( to_key=to_zkey )
		return ArrMult2d( hereToNext, nextToEnd ) #(1326,T)

	#By now we already have π( 𝓹(z),z ) ∀z; ∴ ∀ z∈Z[S], accumulate π(n,z) ∀ n∈𝓟(z) by stepping backward through 𝓟(z)
	cdef void accumulate_zpath_fwd_reaches( CFRCollector self, ll Sr, ll zKey ):

		cdef:
			ll     pKey, stepKey
			flt2   fwdReaches
			GTNode zNode   = self.at( zKey ), parentNode
			uint   rIdx    = zNode.TerminalPath.index_of( Sr )
			ll1    SPz     = zNode.TerminalPath.view()[ rIdx: ] #S∩𝓟(z)
			uint   pathLen = SPz.shape[ 0 ], reverseStep

		for reverseStep from pathLen-1 > reverseStep >= 1:
			stepKey    = SPz[ reverseStep ]
			parentNode = self.GTParent( stepKey )
			pKey       = parentNode.Key

			if parentNode.FwdReaches is None: parentNode.initialize_fwd_reaches() #if parent is opp node
			fwdReaches = self.TerminalFwdReach( from_key=pKey, via_key=stepKey, to_zkey=zKey ) 
			parentNode.FwdReaches.set_weights( to_key=zKey, weights=fwdReaches )

	#∀ z∈Z[S], get 𝓟(z) & calculate π(n,z) ∀ n∈𝓟(z)
	cdef void calculate_subgame_fwd_reaches( CFRCollector self, ll Sr ):
		cdef:
			vector_ll ZS = self.at( Sr ).Zn #Z[S]
			ll        zKey
			uint      z
		for z from 1 <= z <= ZS.size:
			zKey = ZS.at( z-1 )
			self.accumulate_zpath_fwd_reaches( Sr,zKey )

	cdef void calculate_fwd_reaches( CFRCollector self ):

		cdef:
			uint   s, nSS = self.SolvableSubgames.size
			ll     Sr
			double fwdStart = TimeNow(), fwdTime

		print( f"\nCalculating π(n,z) ∀ n,z∈Sₛ ∀ Sₛ∈𝓢ₛ..." )

		for s from 1 <= s <= nSS:
			Sr = self.SolvableSubgames.at( s-1 )
			self.calculate_subgame_fwd_reaches( Sr )
			print( PB( s,nSS ) + f" {s}/{nSS}", end='\r' )
		fwdTime = TimeNow()-fwdStart

		print( f"\nπ(n,z) ∀ n,z∈Sₛ successfully determined ∀ Sₛ∈𝓢ₛ, time taken: {fwdTime:.3f}sec" )

	cdef void calculate_reaches( CFRCollector self ):

		print( '\n'+("="*50) )
		print( f"CALCULATING REACH PROBABILITIES".center(50) )
		print( "="*50 ) 

		cdef double rStart = TimeNow()

		#First do initial mem allocation and base steps for inductive reach calculation 
		self.initialize_fwd_reach_maps() #Find & init πfwd maps for all nodes in all solvable subgames
		self.set_base_fwd_reaches()      #Inductive base step for πfwd
		self.calculate_base_cfreaches()  #Inductive base step for πₒ

		#Now do the hard crunching - ultimately these are the reaches we need for our nn targets
		self.calculate_counterfactual_reaches()
		self.calculate_fwd_reaches()

		cdef double rTime = TimeNow()-rStart

		print( '\n'+("="*50) )
		print( f"ALL REACHES CALCULATED".center(50) )
		print( "="*50 ) 
		print( f"Time taken: {rTime:.3f}s ( +{(rTime/self.SegmentTravsDone):.5f}s to agg avg kTime )" )

	#πₒᵗ(I) = Σ{h∈I}( πₒᵗ(h) )
	cdef flt1 IReach( CFRCollector self, GTNode n ):
		cdef uint HISTORIES=0
		return NP( n.CFReaches.view() ).sum( axis=HISTORIES ) #(T,)

	#πᵗ(h,z)*u(z) ∀ h∈I,z∈Z[I],t<T; Get Z[n] ⟶ ∀z∈Z[n], calculate πᵗ(h,z)*CFU(z)
	cdef flt3 ReachWeightedPayoffs( CFRCollector self, GTNode n ):
		cdef:
			ll   zKey
			uint nZ  = n.Zn.size, nH = NUM_POSSIBLE_HANDS, z
			flt3 piZ = cyarr( (nZ,nH,T), FLTSIZE, 'f' ), uZ3d
			flt1 uZ  = cyarr( (nZ,), FLTSIZE,'f' )
		for z from 1 <= z <= nZ:
			zKey        = n.Zn.at( z-1 )
			uZ[ z-1 ]   = n.UZn.payout_from( zKey )
			piZ[ z-1 ]  = n.FwdReaches.get_weights( to_key=zKey )    #(nZ,1326,T) ⟵ (1326,T)
		uZ3d = newaxis( axis=2, a=newaxis( axis=1, a=NP(uZ) ) )  #(nZ,1,1)
		return ArrMult3d( piZ, uZ3d ) #πᵗ(h,z)*u(z) ∀ h∈I,z∈Z[I],t<T #(nZ,1326,T)

	#vᵗ(I) = Σ{h∈I}( πₒᵗ(h)Σ{z∈Z[I]}( πᵗ(h,z)u(z) ) )
	cdef flt2 NodeValue( CFRCollector self, GTNode n ):
		cdef:
			uint ENDGAMES=0, PATHS=0
			flt3 UZWeighted = self.ReachWeightedPayoffs( n )           #(|Z[I]|,1326,T) #πᵗ(h,z)u(z)    
			flt2 nExpVal    = NP( UZWeighted ).sum( axis=ENDGAMES )    #(1326,T)        #Σ{z∈Z[I]}( πᵗ(h,z)u(z) )
			flt2 vITerms    = ArrMult2d( n.CFReaches.view(), nExpVal ) #(1326,T)
			flt1 vI         = NP( vITerms ).sum( axis=PATHS )          #(T,)
		return newaxis( vI,axis=0 ) #(1,T)

	#vᵗ(I,a) = Σ{h∈I}( πₒᵗ(h)Σ{z∈Z[I·a]}( πᵗ(h·a,z)u(z) ) )
	cdef flt2 ActionValues( CFRCollector self, GTNode n ):

		cdef:
			uint nA = n.A.size, nH=NUM_POSSIBLE_HANDS, ENDGAMES=0, PATHS=1, a
			ll   aKey
			flt3 CFReaches = newaxis( n.CFReaches.view(),axis=0 ) #(1,1326,T)
			flt3 AExpVals  = cyarr( (nA,nH,T), FLTSIZE, 'f' ), aUZWeighted, vIATerms
			flt2 aExpVal

		for a from 1 <= a <= nA:
			aKey            = n.SuccessorKeys.at( a-1 )
			aNode           = self.at( aKey )
			aUZWeighted     = self.ReachWeightedPayoffs( aNode )     #(|Z[I·a]|,1326,T) #πᵗ(h·a,z)u(z) 
			aExpVal         = NP( aUZWeighted ).sum( axis=ENDGAMES ) #(1326,T) #Σ{z}( πᵗ(h·a,z)u(z) )
			AExpVals[ a-1 ] = aExpVal

		vIATerms = ArrMult3d( CFReaches, AExpVals ) #(|A|,1326,T)
		return NP( vIATerms ).sum( axis=PATHS )     #(|A|,T)

	#α(I,a) = Σ{t<T}( tπₒᵗ(I)(vᵗ(I-a)-vᵗ(I)) ) / Σ{t<T}( tπₒᵗ(I) )
	cdef flt1 ActionAdvs( CFRCollector self, GTNode POVnode ):

		cdef:
			uint  ITERS=1
			flt2  tRange             = newaxis( NP( arange( T ) ),axis=0 )                  #(1,T)
			flt2  Ireach             = newaxis( self.IReach( POVnode ),axis=0 )             #(1,T)
			flt2  linReachTerms      = ArrMult2d( tRange, Ireach ) #Retains 0 reaches       #(1,T)
			flt2  reachDenom         = Unzero2d( Ireach ) #Avoid 0div errs. Undone below.   #(1,T)
			flt2  vIA                = ArrDiv2d( self.ActionValues( POVnode ), reachDenom ) #(|A|,T)
			flt2  vI                 = ArrDiv2d( self.NodeValue( POVnode ), reachDenom )    #(1,T)
			flt2  CFRegrets          = ArrSub2d( vIA, vI )                                  #(|A|,T)
			flt2  linRegretTerms     = ArrMult2d( linReachTerms, CFRegrets ) #Undoes unzero #(|A|,T)
			flt1  totalLinearRegrets = sumaxis( linRegretTerms, axis=ITERS, dtype=f32 )     #(|A|,)
			float totalLinearReach   = sumaxis( linReachTerms[0], dtype=f32 )
			uint  nA                 = vIA.shape[ 0 ]
			flt1  aAdvs              = cyarr( (nA,), FLTSIZE, 'f' )

		#METANATION VISUALIZATION
		if totalLinearReach==0: aAdvs[:]=0
		else:                   aAdvs = NP( totalLinearRegrets,dtype=f32 ) / totalLinearReach
		return aAdvs #(|A|,)

	cdef void calculate_subgame_targets( CFRCollector self, GTNode Sr ):

		cdef:
			uint      s
			int1      aInds
			flt1      advIA
			ll        subKey
			GTNode    subNode
			advmap    Isamples
			vector_ll S = Sr.SubKeys

		for s from 1 <= s <= S.size:
			subKey  = S.at( s-1 )
			subNode = self.at( subKey )

			#TODO: Def of aInds here only works if not eliminating actions w strat accums. Fix if reimplement accums
			if subNode.ActingPlayer == self.POVplayer:
				advIA = self.ActionAdvs( subNode )
				aInds = np.arange( subNode.FullAInds.size,dtype=intc )
				Iadvs = advmap( for_iter=T, I=subNode.I_tp, aInds=aInds, aTargets=advIA )
				Iadvs.save_sample_dicts( to_file=self.AdvFile )
				self.nCollectedSamples+=advIA.shape[ 0 ]
				self.nSolvedPositions+=1

	cdef void Calculate_Targets( CFRCollector self ):

		print( '\n\n'+("="*100) )		
		print( f"CALCULATING αNET TARGETS".center(100) )
		print( "="*100 )
		cdef double aStart = TimeNow()

		#First find all solvable paths & set all data necessary for target calculation
		self.build_solvable_paths()
		#Now that probability & payout arrays are all path-aligned, we can accumulate reach probs along those paths
		self.calculate_reaches()

		cdef:
			ll     rKey
			GTNode Sr
			uint   nSS    = self.SolvableSubgames.size, s
			double tStart = TimeNow()

		print( '\n'+("="*50) )
		print( f"DERIVING SUBGAME αTARGETS FROM REACH PROBABILITIES".center(50) )
		print( "="*50 )

		#∀ S∈𝓢ₛ, use accumulated reach probabilities and collected payouts to calculate α(I(n),A) ∀ n∈S
		for s from 1 <= s <= nSS:
			rKey = self.SolvableSubgames.at( s-1 )
			Sr   = self.at( rKey )
			self.calculate_subgame_targets( Sr )
			self.nSolvedSubgames+=1
			print( PB( s,nSS ) + f" {s}/{nSS}", end='\r' )

		cdef double tTime = TimeNow()-tStart, aTime = TimeNow()-aStart
		self.AdvCalcTime  = aTime
		print( f"\nαTargets derived successfully, time taken: {tTime:.3f}s" )

		print( '\n\n'+(f"="*100) )
		print( f"ITERATION {self.SolvingIter} αTARGETS DETERMINED AND SAVED".center(100) )
		print( f"="*100 )
		print( f"|𝓢ₛ| = {nSS}" )
		print( f"α samples saved to: {self.AdvFile}" )
		print( f"Targets collected:  {self.nCollectedSamples}" )
		print( f"Target calc time:   {aTime:.3f}s ( +{(aTime/self.SegmentTravsDone):.5f}s to agg avg kTime )" )
		print( f"Iso avg trav time:  {self.AvgTravTimeIso:.7f}s" )