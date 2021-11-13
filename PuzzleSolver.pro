outputFile('.\\solved\\puzzle_00.txt').
inputFile('.\\unsolved\\puzzle_00.txt').

/********************* solving the puzzle */
doSolve(P, S):-
	transpose(P, S).


temp(puzzle(size(X,Y), board(B)), puzzle(size(X,Y), board(B))).

transpose(puzzle(size(X,Y), board(B)), puzzle(size(Y,X), board(TB))):-
	trans(B, TB).
trans([],[]).
trans([[]|_], []):-!.
trans([S|R], [L|L1]) :-
    trans(S, R, L, M),
    trans(M, L1).

trans([], _,[],[]).
trans([S1|S2], [], [S1|L1], [S2|M]):-
    trans([], [], L1, M).
trans([S1|S2], [R1|R2], [S1|L1], [S2|M]):-
    trans(R1, R2, L1, M).



% Checks that number constraint for all num tiles are correct 
checkNums(puzzle(size(X, Y), board(B), trans(_)), CurrentX, CurrentY) :-
    write("X: "), write(CurrentX), write(", "), write("Y: "), write(CurrentY), nl, 
    getValue(B, CurrentY, CurrentX, V),
	 write("V: "), write(CurrentX),
	adjacentLights(B, V, CurrentX, CurrentY),
	(CurrentX >= X -> 
    	NextX is 1, NextY is CurrentY + 1 ; 
    	NextX is CurrentX + 1, NextY = CurrentY
    ),
	checkNums(puzzle(size(X, Y), board(B), trans(_)), NextX, NextY).
checkNums(puzzle(size(_, Y), board(_), trans(_)), _, CurrentY):-
	not(CurrentY =< Y).


getAdjacentPos(X, Y, R) :-
   X1 is X - 1,
   X2 is X + 1,
   Y1 is Y - 1,
   Y2 is Y + 1,
   append([], [[X1, Y], [X2, Y], [X, Y1], [X, Y2]], R).

% Checks that nr of lights around a num tile is valid
adjacentLights(Board, "1", X, Y):-
    getAdjacentPos(X, Y, Pos),
    write("Posses: "), write(Pos), nl,
    checkAdjacent(Board, Pos).
adjacentLights(_, _, _, _).

checkPos(Board, [X|[Y|_]]) :-
	(getValue(Board, X, Y, R) ->  
    	write(R) 
    ;   write("false") 
    ).
checkAdjacent(_, []).
checkAdjacent(Board, [Pos|Posses]) :-
    checkPos(Board, Pos),
    checkAdjacent(Board, Posses).



getValue(Board, RowNum, ColNum, Val) :- 
    nth1(RowNum, Board, Row), nth1(ColNum, Row, Val).
row(puzzle(size(_, _), board(B), trans(_)), N, Row) :-
    nth1(N, B, Row).
col(puzzle(size(_, _), board(_), trans(BT)), N, Col) :-
    row(puzzle(size(_, _), board(BT), trans(_)), N, Col).

/********************* writing the result */
writeFullOutput(puzzle(size(X,Y), board(Grid))):- 
	write("size "), write(X), write("X"), write(Y), nl,
	writeBoard(Grid).
writeFullOutput(P):- write('Cannot solve puzzle: '), write(P), nl.

writeBoard([]).
writeBoard([H|T]):-
	writeLine(H),
	writeBoard(T).

writeLine([]):- nl.
writeLine([Head|Tail]):- 
	write(Head),
	writeLine(Tail).


/********************** reading the input */
readProblem(puzzle(size(X,Y), board(Grid))) :- 
	findKW(size), 
	readInt(X), 
	readInt(Y), 
	length(Grid, Y),
	readGridLines(X,Grid).

findKW(KW):- 
	string_codes(KW,[H|T]), peek_code(H), readKW([H|T]), !.
findKW(_):- 
	peek_code(-1), !, fail.
findKW(KW):- 
	get_code(_), findKW(KW).


readKW([]):- 
	get_code(_).
readKW([H|T]):- 
	get_code(H), readKW(T).


readGridLines(_,[]).
readGridLines(N,[H|T]):- 
	length(H,N), 
	readGridLine(H), 
	readGridLines(N,T).

readGridLine([]).
readGridLine([E|T]):- 
	get_code(M), 
	translate(M,E), 
	!, 
	readGridLine(T).

translate(-1,'ERROR: EOF').
translate(63,_).
translate(X,E):- whitespace(X), get_code(Y), translate(Y,E).
translate(X,E):- name(E,[X]).


whitespace(10). whitespace(12). whitespace(32).


readHintLine(0).
readHintLine(N):-
	N>0, 
	N1 is N-1, 
	get_code(_), 
	readHintLine(N1).

readLines(_,0).
readLines(X,Y):- 
	Y>0, 
	Y1 is Y-1,
	readHintLine(X), 
	readLines(X,Y1).


readInt(N):- 
	get_code(M),
	handleCode(M,N).


handleCode(M,N):- 
	is_number_code(M,N1),
	!,
	continueInt(N1,N).
handleCode(-1,_):- 
	!, 
	fail. /* EOF */
handleCode(_,N):- 
	readInt(N).


continueInt(O,N):- 
	get_code(M),
	is_number_code(M,M1),
	!,
	H is 10*O+M1,
	continueInt(H,N).
continueInt(N,N).


is_number_code(N, N1):- 
	N>=48,
	N<58,
	N1 is N-48.
is_number_code(95,0).

/*********************** global control: starting the algorithm and the reading */
input_output(IF,OF):- 
	current_prolog_flag(argv, ["--io",IF,OF]), !.
input_output(IF,OF):- 
	inputFile(IF),
	outputFile(OF).
	


run :- 
	input_output(IF, OF),
	write("IO: "), nl,
	write("\tReading from: "), 
	write(IF), nl,
	write("\twriting to: "), 
	write(OF), nl, 
	see(IF), 
	tell(OF), 
	findKW(puzzles), 
	readInt(N),
	write('puzzles '),
	write(N),
	nl, 
	solvePuzzles(N), 
	told, 
	seen,
	!.
	
run :- 
	told,
	seen. /* close the files */

solvePuzzles(0).
solvePuzzles(N) :- 
	N>0,
	readProblem(P),
	doSolve(P, S),
	writeFullOutput(S),
	!,
	N1 is N-1,
	solvePuzzles(N1).

:- run.
%:- halt.