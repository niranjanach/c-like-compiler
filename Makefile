# Compiler and tools
LEX = flex
YACC = yacc
CC = gcc

# Output file name
TARGET = parser

# Intermediate files
LEX_SRC = scanner.l
YACC_SRC = parser.y
LEX_OUT = lex.yy.c
YACC_OUT = y.tab.c
YACC_HEADER = y.tab.h

# Rules
all: $(TARGET)

$(TARGET): $(LEX_OUT) $(YACC_OUT)
	$(CC) -o $@ $(YACC_OUT) $(LEX_OUT) -lfl

$(YACC_OUT): $(YACC_SRC)
	$(YACC) -d $(YACC_SRC)

$(LEX_OUT): $(LEX_SRC)
	$(LEX) $(LEX_SRC)

clean:
	rm -f $(TARGET) $(LEX_OUT) $(YACC_OUT) $(YACC_HEADER)

