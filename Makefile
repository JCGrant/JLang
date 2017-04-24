frontend_dir := src/Frontend
lexer_src := $(frontend_dir)/Lexer.x
parser_src := $(frontend_dir)/Parser.y
lexer_build := $(frontend_dir)/Lexer.hs
parser_build := $(frontend_dir)/Parser.hs

all: frontend

frontend: $(lexer_build) $(parser_build)

$(lexer_build): $(lexer_src)
	alex $(lexer_src)

$(parser_build): $(parser_src)
	happy $(parser_src)