##
## EPITECH PROJECT, 2024
## makefile
## File description:
## glados
##

NAME	=	glados

SRC_TEST	=	./test/

TEST	=	run_tests.sh

all:
	stack build
	cp `stack path --local-install-root`/bin/${NAME}-exe ./${NAME}

clean:
	stack clean

fclean: clean
	$(RM) ${NAME}

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
	hlint src

.PHONY:	all clean fclean re unit functional tests