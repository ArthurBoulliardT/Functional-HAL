##
## EPITECH PROJECT, 2017
## Makefile
## File description:
## repository Makefile
##

RM	=	rm -f

STK	=	stack

NAME_REPO	=	Hal-project

NAME	=	hal

BINPATH	=	.

ROOTPATH	=	./

all:	$(NAME)

$(NAME):
	stack build --copy-bins --local-bin-path $(BINPATH)
	mv $(NAME_REPO)-exe $(NAME)

run:
	$(STK) run

interpret:
	$(STK) ghci

clean:
	$(STK) clean

fclean:	clean
	rm -f $(ROOTPATH)$(NAME)

init:
	$(STK) init

command:
	$(STK) --help

re:	fclean all

.PHONY:	interpret clean fclean init command re
