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

functional:
	${SRC_TEST}${TEST}

tests: unit functional

lint:
	hlint src

.PHONY:	all clean fclean re unit functional tests