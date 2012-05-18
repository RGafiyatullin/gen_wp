
all: test

test: compile
	erl -noshell -pa ebin/ \
	-eval 'gen_wp_example:basic_test()' \
	-s init stop

compile: clean
	@./rebar compile

clean:
	@./rebar clean
