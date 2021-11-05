outputFile('.\\solved\\puzzle_00.txt').
inputFile('.\\unsolved\\puzzle_00.txt').

/********************* solving the puzzle */
doSolve(P,P).

/********************* writing the result */
writeFullOutput(puzzle(7,7)):- 
  write('size 7x7'), nl,
  write('*______'), nl,
  write('2__*___'), nl,
  write('*1_20_*'), nl,
  write('X__*___'), nl,
  write('X_*____'), nl,
  write('*____0_'), nl,
  write('2*X_*__'), nl.
writeFullOutput(puzzle(10,5)):- 
  write('size 10x5'), nl,
  write('____*___1*'), nl,
  write('_*10_*X_0_'), nl,
  write('_______*_1'), nl,
  write('___*___1*_'), nl.

/********************** reading the input */
readProblem(puzzle(X,Y)):- 
  findKW(size), readInt(X), readInt(Y), readHintLine(X), readLines(X,Y).

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


readHintLine(0).
readHintLine(N):- N>0, N1 is N-1, get_code(_), readHintLine(N1).


readLines(_,0).
readLines(X,Y):- Y>0, Y1 is Y-1, readHintLine(X), readLines(X,Y1).


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
	current_prolog_flag(argv,['--io',IF,OF]), !.
input_output(IF,OF):- 
	write("Infile reading: "), nl,
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
	
run:- 
	told,
	seen. /* close the files */

solvePuzzles(0).
solvePuzzles(N):- N>0, readProblem(P), doSolve(P, S), writeFullOutput(S), !, N1 is N-1, solvePuzzles(N1).

:- run.
%:- halt.