##
## EPITECH PROJECT, 2024
## makefile
## File description:
## glados
##

NAME_COMPILER	=	glados
NAME_VM	=	vm

SRC_TEST	=	./compiler/test/

TEST	=	run_tests.sh


all: compiler_exe executer_exe

compiler:
	stack build :${NAME_COMPILER}-exe
	cp `stack path --local-install-root`/bin/${NAME_COMPILER}-exe ./${NAME_COMPILER}

executer:
	stack build :${NAME_VM}-exe
	cp `stack path --local-install-root`/bin/${NAME_VM}-exe ./${NAME_VM}

clean:
	stack clean
	$(RM) -r .stack-work/ .stack-work/executer/ .stack-work/compiler/

fclean: clean
	$(RM) ${NAME_COMPILER} ${NAME_VM}

re:	fclean all

unit:
	stack test

functional: re
	${SRC_TEST}${TEST}

tests: unit functional

coverage:
	cabal configure --enable-coverage
	cabal test
	@file_path=$$(find "./dist-newstyle/build" -path "*/glados-*/hpc/vanilla/html/glados-*/hpc_index.html" | head -n 1); \
	if [ -n "$$file_path" ]; then \
		xdg-open "$$file_path"; \
	else \
		echo "No coverage report found."; \
	fi

lint:
	hlint compiler/lib
	hlint executer/lib

.PHONY:	all clean fclean re unit functional tests compiler executer coverage lint