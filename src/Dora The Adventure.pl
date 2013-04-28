/* IF 2050 - Informatics Logic */
/* Solve The Maze */
/* This program is created for helping Dora in solving the maze */

/* Author of this file */
/* 1. Fathan Adi Pranaya / 13511027 */
/* 2. Yogi Salomo Mangontang Pratama / 13511059 */
/* 3. Habibie Faried / 13511069 */
/* 4. Iskandar Setiadi / 13511073 */

/* Dynamic allocations */
:-dynamic(player_at/2).
/* Single player section */
:- dynamic(current_location/2).
:- dynamic(is_passed/2).
:- dynamic(step/1).
:- dynamic(keadaan/1).
:- dynamic(koin/1).
:- dynamic(cewek/1).
:- dynamic(jumlah_cewek/1).
:- dynamic(movement/1).
:- dynamic(score/1).

/* AI section */
:-dynamic(passed/2).
:-dynamic(ccx/1).
:-dynamic(ccy/1).
:-dynamic(isvisited/2).
:-dynamic(langkah/1).
:-dynamic(isfinished/1).
:-dynamic(mati/2).
:-dynamic(namaladies/1).
:-dynamic(jmlswamp/1).

/* End of Dynamic Allocations */

/* initialization if not exists */
/* pit(-1,-1).
swamp(-1,-1).
coin(-1,-1).
ladies(-1,-1,unknown). */


/* All kinds of player section */
player_at(X,Y) :- start(X,Y).

/* Single player section */
/* Initialize starting facts */
current_location(50,50).
is_passed(0,0).
step(0).
movement(0).
koin(0).
score(0).
cewek([]).
jumlah_cewek(0).
keadaan(hidup).

/* List Operations - Concatenating and inverting list */
konkat([],[], X) :- X = [] ,!.
konkat([],[H2|T2],[H2|X]) :- konkat([],T2,X).
konkat([H1|T1],S,[H1|X]) :- konkat(T1,S,X).
inverse([],L) :-  L = [].
inverse([H|T],L) :-  inverse(T,S), konkat(S,[H],L).

/* Censor definition */
breeze :- current_location(X,Y1), Y is Y1-1, pit(X,Y), write('Alert : Ada Pit coy!'), nl,!.
breeze :- current_location(X,Y1), Y is Y1+1, pit(X,Y), write('Alert : Ada Pit coy!'), nl,!.
breeze :- current_location(X1,Y), X is X1-1, pit(X,Y), write('Alert : Ada Pit coy!'), nl,!.
breeze :- current_location(X1,Y), X is X1+1, pit(X,Y), write('Alert : Ada Pit coy!'), nl,!.
breeze.

strentch :- current_location(X,Y1), Y is Y1-1, swamp(X,Y), write('Alert : Hati-hati nyemplung!'), nl,!.
strentch :- current_location(X,Y1), Y is Y1+1, swamp(X,Y), write('Alert : Hati-hati nyemplung!'), nl,!.
strentch :- current_location(X1,Y), X is X1-1, swamp(X,Y), write('Alert : Hati-hati nyemplung'), nl,!.
strentch :- current_location(X1,Y), X is X1+1, swamp(X,Y), write('Alert : Hati-hati nyemplung'), nl,!.
strentch.

scream :- current_location(X,Y1), Y is Y1-1, ladies(X,Y,_), \+is_passed(X,Y), write('Alert : Cuit cuit cewek godain dong!'), nl,!.
scream :- current_location(X,Y1), Y is Y1+1, ladies(X,Y,_), \+is_passed(X,Y), write('Alert : Cuit cuit cewek godain dong!'), nl,!.
scream :- current_location(X1,Y), X is X1-1, ladies(X,Y,_), \+is_passed(X,Y), write('Alert : Cuit cuit cewek godain dong!'), nl,!.
scream :- current_location(X1,Y), X is X1+1, ladies(X,Y,_), \+is_passed(X,Y), write('Alert : Cuit cuit cewek godain dong!'), nl,!.
scream.

glitter :- current_location(X,Y1), Y is Y1-1, coin(X,Y), \+is_passed(X,Y), write('Alert : Ada uang receh tuh!'), nl,!.
glitter :- current_location(X,Y1), Y is Y1+1, coin(X,Y), \+is_passed(X,Y), write('Alert : Ada uang receh tuh!'), nl,!.
glitter :- current_location(X1,Y), X is X1-1, coin(X,Y), \+is_passed(X,Y), write('Alert : Ada uang receh tuh!'), nl,!.
glitter :- current_location(X1,Y), X is X1+1, coin(X,Y), \+is_passed(X,Y), write('Alert : Ada uang receh tuh!'), nl,!.
glitter.

/* Printing stats and condition */
printlocation :- ((current_location(50,50))
				-> write('')
				; write('Posisi anda ada pada: '), current_location(X,Y), write(X), write(','),write(Y)).
count_step :- step(Z), write('Anda sudah melangkah sebanyak : '), write(Z), nl, !.
count_movement :- movement(Z), write('Nilai Movement: '), write(Z), nl, !.
cetak_keadaan :- keadaan(X), write('Sekarang dora sudah : '), write(X), nl, !.
cetak_cewek :- write('Jumlah Cewek: '), jumlah_cewek(N), write(N), nl, write('List Cewek  : '), cewek(C), write(C), nl, !.
count_coin :- write('Jumlah Koin : '), koin(K), write(K), nl, !.
count_score :- koin(Coin), jumlah_cewek(Cewek), movement(Move), Cowek is Coin + Cewek, TotalMove is Move * 5, ToPoint is Cowek * 50, Total is ToPoint - TotalMove, write('Score Anda adalah : '), write(Total), nl, !.
statistik :- count_coin , count_step, count_movement, cetak_cewek, count_score, cetak_keadaan.

/* To start a new game when lose / win condition */
startG :- start(Xawal, Yawal), retractall(current_location(_,_)), retractall(step(_)), retractall(koin(_)), retractall(keadaan(_)), retractall(movement(_)), retractall(cewek(_)), retractall(jumlah_cewek(_)), retractall(score(_)), retractall(is_passed(_,_)), asserta(current_location(Xawal,Yawal)), asserta(step(0)), asserta(movement(0)), asserta(koin(0)), asserta(keadaan(hidup)), asserta(cewek([])), asserta(jumlah_cewek(0)), asserta(score(0)), write('Selamat datang di Game Maze'), nl, printlocation,!.
death :-  retractall(keadaan(_)), asserta(keadaan(mati)), !.
menang :- retractall(keadaan(_)), asserta(keadaan(menang)), write('Selamat anda menang'), !.

/* For each new coordinate conditions */
status(X,Y) :- pit(X,Y), write('Anda jatuh kedalam pit. Anda telah mati! Silahkan coba lagi~ '), nl, death,!.
status(X,Y) :- swamp(X,Y), write('Anda nyemplung ke kali. Basah basah basah ah ah ah'), nl, !.
status(X,Y) :- ladies(X,Y,Nama), \+is_passed(X,Y), write('dapet cewek'), tambah_cewek(Nama), nl, !.
status(X,Y) :- coin(X,Y), \+is_passed(X,Y), write('dapet coin'), koin(K), K1 is K+1, retract(koin(K)), asserta(koin(K1)), nl, !.
status(X,Y) :- finish(X,Y), menang, nl, !.
status(_,_).

tambah_cewek(L) :- cewek(X), inverse([L|X],Lout), inverse(Lout,Lout2), retractall(cewek(_)), asserta(cewek(Lout2)), jumlah_cewek(C), C1 is C+1, retract(jumlah_cewek(C)), asserta(jumlah_cewek(C1)).

/* Dora's movement */
move_to(X,Y) :- keadaan(hidup), retract(current_location(_,_)), asserta(current_location(X,Y)).
is_moveable(X,Y) :- path(X,Y).
is_moveable(X,Y) :- start(X,Y).
is_moveable(X,Y) :- swamp(X,Y).
is_moveable(X,Y) :- coin(X,Y).
is_moveable(X,Y) :- ladies(X,Y,_).
is_moveable(X,Y) :- pit(X,Y).
is_moveable(X,Y) :- finish(X,Y).
moveup :- current_location(X,Y1), step(Z), movement(Z2), Y is Y1-1, 
			(is_moveable(X,Y) -> retract(step(Z)), retract(movement(Z2)), move_to(X,Y), 
			(swamp(X,Y) ->  Z1 is Z2+5, Z3 is Z+1
			; Z1 is Z2+1, Z3 is Z+1),asserta(step(Z3)), asserta(movement(Z1)), glitter, strentch, scream, breeze, nl, status(X,Y), nl, assertz(is_passed(X,Y)), printlocation
			; write('nggak bisa kesana cuy')).

movedown :- current_location(X,Y1), step(Z), movement(Z2), Y is Y1+1, 
			(is_moveable(X,Y) -> retract(step(Z)), retract(movement(Z2)), move_to(X,Y),  
			(swamp(X,Y) ->  Z1 is Z2+5, Z3 is Z+1
			; Z1 is Z2+1, Z3 is Z+1),asserta(step(Z3)), asserta(movement(Z1)), glitter, strentch, scream, breeze, nl, status(X,Y), nl, assertz(is_passed(X,Y)), printlocation
			; write('nggak bisa kesana cuy')).
moveleft :- current_location(X1,Y), step(Z), X is X1-1, movement(Z2), 
			(is_moveable(X,Y) -> retract(step(Z)), retract(movement(Z2)), move_to(X,Y),
			(swamp(X,Y) ->  Z1 is Z2+5, Z3 is Z+1
			; Z1 is Z2+1, Z3 is Z+1),asserta(step(Z3)), asserta(movement(Z1)), glitter, strentch, scream, breeze, nl, status(X,Y), nl, assertz(is_passed(X,Y)), printlocation
			; write('nggak bisa kesana cuy')).
moveright :- current_location(X1,Y), step(Z), X is X1+1, movement(Z2), 
			(is_moveable(X,Y) -> retract(step(Z)), retract(movement(Z2)), move_to(X,Y),
			(swamp(X,Y) ->  Z1 is Z2+5, Z3 is Z+1
			; Z1 is Z2+1, Z3 is Z+1),asserta(step(Z3)), asserta(movement(Z1)), glitter, strentch, scream, breeze, nl, status(X,Y), nl, assertz(is_passed(X,Y)), printlocation
			; write('nggak bisa kesana cuy')).

/* End of Single player section */

/* AI section */

true(1).

solve :- start(X,Y),
		  retractall(passed(_,_)),
		  retractall(ccx(_)),
		  retractall(ccy(_)),
		  retractall(isvisited(_,_)),
		  retractall(langkah(_)),
		  retractall(isfinished(_)),
		  retractall(mati(_,_)),
		  retractall(namaladies(_)),
		  retractall(jml_cewek(_)),
		  retractall(koin(_)),
		  retractall(jmlswamp(_)),
		  asserta(jmlswamp(0)),
		  asserta(koin(0)),
		  asserta(jml_cewek(0)),
		  asserta(namaladies([])),
		 asserta(langkah(0)),
		 asserta(mati(-1,-1)),  /* Initial Existential definition */
		 asserta(isvisited(X,Y)), 
		 asserta(ccx(X)),
	     asserta(ccy(Y)),
		 asserta(isfinished(0)),
		 solvemaze.

solveagain :- start(X,Y),
		  retractall(jmlswamp(_)),
		  retractall(passed(_,_)),
 		  retractall(ccx(_)),
		  retractall(ccy(_)),
		  retractall(isvisited(_,_)),
		  retractall(langkah(_)),
		  retractall(jml_cewek(_)),
		  retractall(namaladies(_)),
		  retractall(koin(_)),
		  retractall(isfinished(_)),
		  asserta(jmlswamp(0)),
		  asserta(jml_cewek(0)),
		  asserta(koin(0)),
		  asserta(namaladies([])),
		 asserta(langkah(0)),
		 asserta(isvisited(X,Y)), 
		 asserta(ccx(X)),
	     asserta(ccy(Y)),
		 asserta(isfinished(0)),
		 solvemaze.

countlangkah(X) :- ((X = 1)
                    ->	(langkah(L)), (L1 is L+1), (retractall(langkah(_))), (asserta(langkah(L1)))
					; (langkah(L), L1 is L-1, retractall(langkah(_)), asserta(langkah(L1)))
				   ).

print_p :- ccx(X), ccy(Y), write('('), print(X), write(','), print(Y), write(')'), nl,!.

validpath(A,B) :- path(A,B).
validpath(A,B) :- start(A,B).
validpath(A,B) :- finish(A,B).

validunmove(A,B) :- validpath(A,B).
validunmove(A,B) :- coin(A,B).
validunmove(A,B) :- swamp(A,B).
validunmove(A,B) :- ladies(A,B,_).


validmove(A,B) :- (  (validpath(A,B), (\+ isvisited(A,B)), isfinished(0))
					  -> (asserta(isvisited(A,B)), retractall(ccx(_)), retractall(ccy(_)), asserta(ccx(A)),asserta(ccy(B)), assertz(passed(A,B)))
					  ; ((pit(A,B), (\+ isvisited(A,B)), (\+ mati(A,B)), isfinished(0))
						 -> (asserta(mati(A,B)), asserta(isvisited(A,B)), retractall(ccx(_)), retractall(ccy(_)), asserta(ccx(A)),asserta(ccy(B)), solveagain)
						 ; ((coin(A,B), (\+ isvisited(A,B)), isfinished(0))
							-> (asserta(isvisited(A,B)), retractall(ccx(_)), retractall(ccy(_)), asserta(ccx(A)),asserta(ccy(B)), assertz(passed(A,B)))
							; ((swamp(A,B), (\+ isvisited(A,B)), isfinished(0))
							  -> (jmlswamp(S), S1 is S+1, retractall(jmlswamp(_)), asserta(jmlswamp(S1)), asserta(isvisited(A,B)), retractall(ccx(_)), retractall(ccy(_)), asserta(ccx(A)),asserta(ccy(B)), assertz(passed(A,B)))
							  ; ((ladies(A,B,Y), (\+ isvisited(A,B)), isfinished(0))
								-> ( namaladies(X), Xt = X, retractall(namaladies(_)), inverse([Y|Xt],L), inverse(L,L2), asserta(namaladies(L2)), asserta(isvisited(A,B)), retractall(ccx(_)), retractall(ccy(_)), asserta(ccx(A)),asserta(ccy(B)), assertz(passed(A,B)))
								; fail
								)
							  )
						   )
						 )
				   ).
				   
move(1) :- ccx(X),
           ccy(Y),
		   A is X-1, 
		   B is Y, 
		   validmove(A,B),!.

move(2) :- ccx(X),
           ccy(Y),
		   A is X, 
		   B is Y+1, 
		   validmove(A,B),!.

move(3) :- ccx(X),
           ccy(Y),
		   A is X+1, 
		   B is Y,
		   validmove(A,B),!.
		   
move(4) :- ccx(X),
           ccy(Y),
		   A is X, 
		   B is Y-1, 
		   validmove(A,B),!.

/* End of Move */

/* unmove up -> move to bottom */
unmove(1) :- (
			(isfinished(0))
			->
			(ccx(X),
			 ccy(Y),
			 retract(passed(X,Y)),
			 validunmove(A,B),
			 retractall(ccx(_)),
			 retractall(ccy(_)),
		     A is X + 1,
			 B is Y, 
			 countlangkah(0),
			 asserta(ccx(A)),
			 asserta(ccy(B)),!
			)
			; true(1)
			).

/* unmove right -> move to left */
unmove(2) :- ( 
             (isfinished(0))
			 ->
				(
				 ccx(X),
				 ccy(Y),
				 retract(passed(X,Y)),
				 validunmove(B,A),
				 retractall(ccx(_)),
				 retractall(ccy(_)),
				 A is Y - 1,
				 B is X,
				 countlangkah(0),
				 asserta(ccx(B)),
				 asserta(ccy(A)),!
				)
			; true(1)
			) .
				 
/* unmove bottom -> move to up */
unmove(3) :- ( 
			  (isfinished(0))
			  ->
			  (
			  ccx(X),
			 ccy(Y),
			 retract(passed(X,Y)),
			 retractall(ccx(_)),
			 retractall(ccy(_)),
			 A is X - 1,
			 B is Y,
			 validunmove(A,B),
			 countlangkah(0),
		     asserta(ccx(A)),
			 asserta(ccy(B)),!
			 )
			 ; true(1)
			 ).

/* unmove left -> move to right */
unmove(4) :- (
			  (isfinished(0))
			 ->
			 (
			 ccx(X),
			 ccy(Y),
			 retract(passed(X,Y)),
			 validunmove(B,A),
			 retractall(ccx(_)),
			 retractall(ccy(_)),
		     A is Y + 1,
			 B is X,
			 countlangkah(0),
		     asserta(ccx(B)),
			 asserta(ccy(A)),!
			 )
			 ; true(1)
			 ).
			
/* Base, solvemaze = true iff ccx && ccy = finish */

printAllPassed(C,D) :- write('Solution : '), write(C), write(' '), write(D), 
						retract(current_location(_,_)), asserta(current_location(C,D)) , nl,
						glitter, strentch, scream, breeze, nl, status(C,D), nl, assertz(is_passed(C,D)),retractall(ccx(_)), retractall(ccy(_)), asserta(ccx(C)), asserta(ccy(D)), 
						checkAtas(C,D), checkKiri(C,D), checkKanan(C,D), checkBawah(C,D), nl.
						
/* Additional check */
/* if such a move exists */
isExist(A,B) :- path(A,B), !.
isExist(A,B) :- coin(A,B), !.
isExist(A,B) :- ladies(A,B,_), !.
isExist(A,B) :- swamp(A,B), !.
isExist(A,B) :- pit(A,B), !.

checkAtas(A,B) :- ( (\+ finish(A,B))
					 -> ( (C is A, D is B-1, isExist(C,D))
						-> retract(current_location(_,_)), asserta(current_location(C,D)), (
						  ( (ladies(C,D,Y), (\+ passed(C,D)))
							->  namaladies(X), Xt = X, retractall(namaladies(_)), inverse([Y|Xt],L), inverse(L,L2), asserta(namaladies(L2)), asserta(isvisited(C,D)), countlangkah(1), asserta(passed(C,D)), printAllPassed(C,D) , printAllPassed(A,B), countlangkah(1)
							;( (coin(C,D), (\+ passed(C,D)))
								-> asserta(isvisited(C,D)), countlangkah(1), asserta(passed(C,D)), printAllPassed(C,D) , printAllPassed(A,B), countlangkah(1), (
								  (swamp(A,B))
								  -> jmlswamp(T), Ti is T + 1, retractall(jmlswamp(_)), asserta(jmlswamp(Ti))
								  ; true(1)
								)
								; true(1)
							)
						  )
						)
						; true(1)
					    )
					  ; true(1)
					).
					
checkKiri(A,B) :- ( (\+ finish(A,B))
					 -> ( (C is A-1, D is B, isExist(C,D))
						-> retract(current_location(_,_)), asserta(current_location(C,D)), (
						  ( (ladies(C,D,Y), (\+ passed(C,D)))
							->  namaladies(X), Xt = X, retractall(namaladies(_)), inverse([Y|Xt],L), inverse(L,L2), asserta(namaladies(L2)), asserta(isvisited(C,D)), countlangkah(1), asserta(passed(C,D)), printAllPassed(C,D) , printAllPassed(A,B), countlangkah(1)
							;( (coin(C,D), (\+ passed(C,D)))
								-> asserta(isvisited(C,D)), countlangkah(1), asserta(passed(C,D)), printAllPassed(C,D) , printAllPassed(A,B), countlangkah(1), (
								  (swamp(A,B))
								  -> jmlswamp(T), Ti is T + 1, retractall(jmlswamp(_)), asserta(jmlswamp(Ti))
								  ; true(1)
								)
								; true(1)
							)
						  )
						)
						; true(1)
					    )
					  ; true(1)
					).

checkKanan(A,B) :- ( (\+ finish(A,B))
					 -> ( (C is A+1, D is B, isExist(C,D))
						-> retract(current_location(_,_)), asserta(current_location(C,D)), (
						  ( (ladies(C,D,Y), (\+ passed(C,D)))
							->  namaladies(X), Xt = X, retractall(namaladies(_)), inverse([Y|Xt],L), inverse(L,L2), asserta(namaladies(L2)), asserta(isvisited(C,D)), countlangkah(1), asserta(passed(C,D)), printAllPassed(C,D) , printAllPassed(A,B), countlangkah(1)
							;( (coin(C,D), (\+ passed(C,D)))
								-> asserta(isvisited(C,D)), countlangkah(1), asserta(passed(C,D)), printAllPassed(C,D) , printAllPassed(A,B), countlangkah(1), (
								  (swamp(A,B))
								  -> jmlswamp(T), Ti is T + 1, retractall(jmlswamp(_)), asserta(jmlswamp(Ti))
								  ; true(1)
								)
								; true(1)
							)
						  )
						)
						; true(1)
					    )
					  ; true(1)
					).
					
checkBawah(A,B) :- ( (\+ finish(A,B))
					 -> ( (C is A, D is B+1, isExist(C,D))
						-> retract(current_location(_,_)), asserta(current_location(C,D)), (
						  ( (ladies(C,D,Y), (\+ passed(C,D)))
							->  namaladies(X), Xt = X, retractall(namaladies(_)), inverse([Y|Xt],L), inverse(L,L2), asserta(namaladies(L2)), asserta(isvisited(C,D)), countlangkah(1), asserta(passed(C,D)), printAllPassed(C,D) , printAllPassed(A,B), countlangkah(1)
							;( (coin(C,D), (\+ passed(C,D)))
								-> asserta(isvisited(C,D)), countlangkah(1), asserta(passed(C,D)), printAllPassed(C,D) , printAllPassed(A,B), countlangkah(1), (
								  (swamp(A,B))
								  -> jmlswamp(T), Ti is T + 1, retractall(jmlswamp(_)), asserta(jmlswamp(Ti))
								  ; true(1)
								)
								; true(1)
							)
						  )
						)
						; true(1)
					    )
					  ; true(1)
					).

/* Base Case */
solvemaze :- ccx(X),
			 ccy(Y),
			 A is X,
			 B is Y,
			 (isfinished(0)
			 ->  ( (finish(A,B))
					-> (true(1), retractall(is_passed(_,_)), forall(passed(C,D),printAllPassed(C,D)), nl, printPassed,
					langkah(E), retractall(step(_)), asserta(step(E)),
					jmlswamp(F), retractall(movement(_)), G is (F * 4) + E, asserta(movement(G)),
					retractall(keadaan(_)), asserta(keadaan(menang)),
					statistik, retractall(isfinished(_)), asserta(isfinished(1)))
					; (solvearah(1))
				 )
				 ; true(1)
			 ).
/* Rekursi */

/* DFS Algorithm */
solvearah(Z) :- (
					ccx(X), ccy(Y), A is X, B is Y,
					retractall(ccx(_)), retractall(ccy(_)), asserta(ccx(A)), asserta(ccy(B)),
					(Z =< 4)
					-> ((
						((move(Z))
						-> ((countlangkah(1)),
							((solvemaze) 
							   -> (true(1)) 
							   ;  (unmove(Z))
							)
						   )
						; write('')
						)
					),
					N is Z + 1,
					solvearah(N)
					)
					; ((isfinished(1))
						-> true(1)
						; fail
					  )
				).

				
/* End of AI section */		
		
/* Print Solution Section */
/* Note for Programmer: printFormatPath has 3 parameters, which is X = file, (Y,Z) = position */
/* In this section, you need to print 2 kinds of thing, the entire map, and player solution */
printMazeSize(X,Y,Z) :- write(X, 'Maze : '), write(X,Y), write(X,' '), write(X,Z), nl(X).

printStartPosition(X,Y,Z) :- write(X, 'Start : '), write(X,Y), write(X,' '), write(X,Z), nl(X).

printFinishPosition(X,Y,Z) :- write(X, 'Finish : '), write(X,Y), write(X,' '), write(X,Z), nl(X).

printFormatPath(X,Y,Z) :- write(X, 'Path : '), write(X,Y), write(X,' '), write(X,Z), nl(X).

printFormatPit(X,Y,Z) :- write(X, 'Pit : '), write(X,Y), write(X,' '), write(X,Z), nl(X).

printFormatSwamp(X,Y,Z) :- write(X, 'Swamp : '), write(X,Y), write(X,' '), write(X,Z), nl(X).

printFormatCoin(X,Y,Z) :- write(X, 'Coin : '), write(X,Y), write(X,' '), write(X,Z), nl(X).

printFormatLadies(X,Y,Z,Nama) :- write(X, 'Ladies : '), write(X,Y), write(X,' '), write(X,Z), write(X,' '), write(X,Nama), nl(X).

printIsVisited(X,Y,Z) :- write(X, 'Solution : '), write(X,Y), write(X,' '), write(X,Z), nl(X).

printCurrentPosition(X,Y,Z) :- write(X, 'Position : '), write(X,Y), write(X,' '), write(X,Z), nl(X).

/* print to animation file */
printG:- open('MyOutput.txt',write,ID),
		maze(A,B),printMazeSize(ID,A,B),
		start(C,D),printStartPosition(ID,C,D),
		finish(E,F),printFinishPosition(ID,E,F),
		forall(path(G,H),printFormatPath(ID,G,H)),
		forall(pit(I,J),printFormatPit(ID,I,J)),
		forall(swamp(K,L),printFormatSwamp(ID,K,L)),
		forall(coin(M,N),printFormatCoin(ID,M,N)),
		forall(ladies(O,P,Nama),printFormatLadies(ID,O,P,Nama)),
		forall(is_passed(Q,R),printIsVisited(ID,Q,R)),
		current_location(S,T),printCurrentPosition(ID,S,T),
		close(ID).

	
/* after AI finished */
printOut(ID,A,B) :- write(ID,'passed('), write(ID,A), write(ID,','), write(ID,B), write(ID,').'), write(ID,' % Pasangan koordinat path yang dilalui'), nl(ID).

printPassed:- open('out.pl',write,ID),
			  forall(is_passed(A,B), printOut(ID,A,B)),
			  close(ID).