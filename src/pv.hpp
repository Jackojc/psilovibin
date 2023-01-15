#ifndef PV_HPP
#define PV_HPP

/*
	HEADERS

	#include <vector>
	#include <unordered_set>
	#include <iostream>
*/

// Errors
namespace pv {
	#define ERROR_KINDS \
		X(GENERIC,         "an error occurred") \
		X(UNREACHABLE,     "unreachable code") \
		X(NOT_IMPLEMENTED, "not implemented") \
		X(UNKNOWN_CHAR,    "unknown character") \
		X(UNDEFINED,       "undefined") \
		\
		X(UNEXPECTED_TOKEN, "unexpected token") \
		\
		X(EXPECT_COMMAND,    "expected a command") \
		X(EXPECT_EXPRESSION, "expected an expression") \
		X(EXPECT_INTEGER,    "expected an integer") \
		X(EXPECT_STRING,     "expected a string") \
		X(EXPECT_LITERAL,    "expected a literal") \
		X(EXPECT_LET,        "expected let") \
		X(EXPECT_IDENTIFIER, "expected an identifier") \
		X(EXPECT_CONTROL,    "expected control") \
		X(EXPECT_INSTRUMENT, "expected instrument")

	#define X(a, b) a,
		enum class ErrorKind: size_t { ERROR_KINDS };
	#undef X

	namespace detail {
		#define X(a, b) b,
			constexpr const char* ERROR_TO_STR[] = { ERROR_KINDS };
		#undef X

		constexpr const char* error_to_str(ErrorKind x) {
			return ERROR_TO_STR[static_cast<size_t>(x)];
		}
	}

	inline std::ostream& operator<<(std::ostream& os, ErrorKind x) {
		return print(os, detail::error_to_str(x));
	}

	struct Report {
		View sv;
		ErrorKind kind;
	};

	template <typename... Ts>
	[[noreturn]] constexpr void report(View sv, ErrorKind x) {
		throw Report { sv, x };
	}

	inline std::ostream& report_handler(std::ostream& os, Report x) {
		return empty(x.sv) ?
			println(os, "[fail] ", x.kind):
			println(os, "[fail] `", x.sv, "` ", x.kind);
	}

	inline std::ostream& operator<<(std::ostream& os, Report x) {
		return report_handler(os, x);
	}
}

// Symbol Table
namespace pv {
	using SymbolTable = std::unordered_set<std::string>;  // TODO: Use a prefix/radix tree
}

// Lexer
namespace pv {
	constexpr bool is_visible(View sv) {
		char x = chr(sv);
		return x >= 33 and x <= 126;
	}

	constexpr bool is_control(View sv) {
		char x = chr(sv);
		return x >= 0 and x <= 31;
	}

	constexpr bool is_alpha(View sv) {
		char x = chr(sv);
		return any(x >= 'a' and x <= 'z', x >= 'A' and x <= 'Z', x == '_');
	}

	constexpr bool is_whitespace(View sv) {
		char x = chr(sv);
		return any(x >= 9 and x <= 13, x == ' ');
	}

	constexpr bool is_digit(View sv) {
		char x = chr(sv);
		return x >= '0' and x <= '9';
	}

	constexpr bool is_alphanumeric(View sv) {
		return is_alpha(sv) or is_digit(sv);
	}

	#define SYMBOL_KINDS \
		X(NONE, "None") \
		X(TERM, "EOF") \
		X(END,  "End") \
		\
		X(IDENTIFIER, "Identifier") \
		X(INTEGER,    "Integer") \
		X(STRING,     "String") \
		X(COMMA,      "Comma") \
		\
		X(GO,       "Go") \
		X(STOP,     "Stop") \
		X(LEFT,     "Left") \
		X(RIGHT,    "Right") \
		X(VELOCITY, "Velocity") \
		X(BPM,      "BPM") \
		X(TIME,     "Time") \
		\
		X(LET,        "Let") \
		X(INSTRUMENT, "Instrument") \
		X(CONTROL,    "Control") \
		X(ACTION,     "Action") \
		X(PROGRAM,    "Program")

	#define X(a, b) a,
		enum class SymbolKind: size_t { SYMBOL_KINDS };
	#undef X

	namespace detail {
		#define X(a, b) b,
			constexpr const char* SYMBOL_TO_STR[] = { SYMBOL_KINDS };
		#undef X

		constexpr const char* symbol_to_str(SymbolKind x) {
			return detail::SYMBOL_TO_STR[static_cast<size_t>(x)];
		}
	}

	inline std::ostream& operator<<(std::ostream& os, SymbolKind x) {
		return print(os, detail::symbol_to_str(x));
	}

	struct Symbol {
		View sv;
		SymbolKind kind;

		constexpr Symbol(View sv_, SymbolKind kind_):
			sv(sv_), kind(kind_) {}
	};

	inline std::ostream& operator<<(std::ostream& os, Symbol x) {
		return print(os, '{', x.kind, ",'", x.sv, "'}");
	}

	constexpr bool operator==(Symbol lhs, Symbol rhs) {
		return lhs.kind == rhs.kind and lhs.sv == rhs.sv;
	}

	constexpr bool operator!=(Symbol lhs, Symbol rhs) {
		return not(lhs == rhs);
	}

	struct Lexer {
		View src;
		View sv;

		Symbol peek;
		Symbol prev;

		Lexer(View src_):
			src(src_), sv(src_),
			peek(pv::peek(src_), SymbolKind::NONE),
			prev(pv::peek(src_), SymbolKind::NONE) {}
	};

	[[nodiscard]] inline Symbol take(Lexer& lx, const SymbolTable& syms) {
		View ws = take_while(lx.sv, is_whitespace);
		Symbol sym { peek(lx.sv), SymbolKind::NONE };

		if (empty(lx.sv))  // EOF
			sym.kind = SymbolKind::TERM;

		else if (sym.sv == ","_sv) { sym.kind = SymbolKind::COMMA; lx.sv = next(lx.sv); }

		else if (sym.sv == "\""_sv) {
			sym.kind = SymbolKind::STRING;
			lx.sv = next(lx.sv);

			sym.sv = take_while(lx.sv, [] (View sv) {
				return sv != "\"";
			});

			lx.sv = next(lx.sv);
		}

		else if (is_digit(sym.sv)) {
			sym.kind = SymbolKind::INTEGER;
			sym.sv = take_while(lx.sv, is_digit);
		}

		else if (is_alpha(sym.sv)) {  // Identifiers.
			sym.kind = SymbolKind::IDENTIFIER;
			sym.sv = take_while(lx.sv, is_alphanumeric);

			if (begins_with(sym.sv, "#"_sv)) {  // Comments.
				View comment = take_while(lx.sv, [] (View sv) {
					return sv != "\n"_sv;
				});

				lx.sv = next(lx.sv);
				return take(lx, syms);
			}

			// TODO: Use incremental matching for keywords/symbols.

			if      (sym.sv == "go"_sv)       sym.kind = SymbolKind::GO;
			else if (sym.sv == "stop"_sv)     sym.kind = SymbolKind::STOP;
			else if (sym.sv == "left"_sv)     sym.kind = SymbolKind::LEFT;
			else if (sym.sv == "right"_sv)    sym.kind = SymbolKind::RIGHT;
			else if (sym.sv == "velocity"_sv) sym.kind = SymbolKind::VELOCITY;
			else if (sym.sv == "bpm"_sv)      sym.kind = SymbolKind::BPM;
			else if (sym.sv == "time"_sv)     sym.kind = SymbolKind::TIME;

			else if (sym.sv == "let"_sv)   sym.kind = SymbolKind::LET;
			else if (sym.sv == "instr"_sv) sym.kind = SymbolKind::INSTRUMENT;
			else if (sym.sv == "ctrl"_sv)  sym.kind = SymbolKind::CONTROL;
		}

		else
			// TODO: This fails and prints nothing (visibly) if character is UTF-8.
			report(sym.sv, ErrorKind::UNKNOWN_CHAR);

		Symbol out = lx.peek;

		lx.prev = lx.peek;
		lx.peek = sym;

		return out;
	}

	constexpr decltype(auto) is(SymbolKind kind) {
		return [=] (Symbol other) { return kind == other.kind; };
	}

	template <typename F> constexpr void expect(Lexer& lx, F&& fn, ErrorKind x) {
		// PV_LOG(LogLevel::WRN, lx.peek);

		if (not fn(lx.peek))
			report(lx.peek.sv, x);
	}
}

// Parser
namespace pv {
	struct Context {
		SymbolTable syms;
	};

	constexpr bool is_literal(Symbol x) {
		return cmp_any(x.kind,
			SymbolKind::INTEGER,
			SymbolKind::STRING);
	}

	constexpr bool is_command(Symbol x) {
		return cmp_any(x.kind,
			SymbolKind::GO,
			SymbolKind::STOP,
			SymbolKind::LEFT,
			SymbolKind::RIGHT,
			SymbolKind::VELOCITY,
			SymbolKind::BPM,
			SymbolKind::TIME);
	}

	constexpr bool is_expr(Symbol x) {
		return is_command(x) or cmp_any(x.kind,
			SymbolKind::IDENTIFIER,
			SymbolKind::CONTROL,
			SymbolKind::INSTRUMENT,
			SymbolKind::LET);
	}

	inline std::vector<Symbol> program(Context&, Lexer&);
	inline std::vector<Symbol> expression(Context&, Lexer&);

	inline std::vector<Symbol> control(Context&, Lexer&);
	inline std::vector<Symbol> instrument(Context&, Lexer&);
	inline std::vector<Symbol> let(Context&, Lexer&);

	inline std::vector<Symbol> action(Context&, Lexer&);
	inline std::vector<Symbol> command(Context&, Lexer&);

	inline Symbol midi(Context&, Lexer&);
	inline Symbol literal(Context&, Lexer&);


	inline std::vector<Symbol> program(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::INF);

		std::vector<Symbol> tree;
		tree.emplace_back(lx.peek.sv, SymbolKind::PROGRAM);

		expect(lx, is_expr, ErrorKind::EXPECT_EXPRESSION);
		std::vector<Symbol> expr = expression(ctx, lx);

		tree.insert(tree.end(), expr.begin(), expr.end());

		if (lx.peek.kind == SymbolKind::COMMA) {
			Symbol comma = take(lx, ctx.syms);

			expect(lx, is_expr, ErrorKind::EXPECT_EXPRESSION);
			std::vector<Symbol> expr = expression(ctx, lx);

			tree.insert(tree.end(), expr.begin(), expr.end());
		}

		expect(lx, is(SymbolKind::TERM), ErrorKind::UNEXPECTED_TOKEN);

		tree.emplace_back(lx.peek.sv, SymbolKind::END);

		return tree;
	}

	inline std::vector<Symbol> expression(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::INF);

		switch (lx.peek.kind) {
			case SymbolKind::LET:        return let(ctx, lx);
			case SymbolKind::INSTRUMENT: return instrument(ctx, lx);
			case SymbolKind::CONTROL:    return control(ctx, lx);

			case SymbolKind::IDENTIFIER: return action(ctx, lx);

			default: {
				report(lx.peek.sv, ErrorKind::EXPECT_EXPRESSION);
			} break;

		}

		report(lx.peek.sv, ErrorKind::UNREACHABLE);
		return {};
	}


	inline std::vector<Symbol> control(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::INF);

		expect(lx, is(SymbolKind::CONTROL), ErrorKind::EXPECT_CONTROL);
		Symbol ctrl = take(lx, ctx.syms);

		expect(lx, is(SymbolKind::IDENTIFIER), ErrorKind::EXPECT_IDENTIFIER);
		Symbol ident = take(lx, ctx.syms);

		std::vector<Symbol> tree;
		tree.emplace_back(ident.sv, SymbolKind::CONTROL);

		// TODO: Read multiple identifier-number pairs.

		// TODO: Expect either number or identifier here.
		expect(lx, is(SymbolKind::INTEGER), ErrorKind::EXPECT_INTEGER);
		Symbol integer = take(lx, ctx.syms);
		tree.push_back(integer);

		tree.emplace_back(lx.prev.sv, SymbolKind::END);

		return tree;
	}

	inline std::vector<Symbol> instrument(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::INF);

		expect(lx, is(SymbolKind::INSTRUMENT), ErrorKind::EXPECT_INSTRUMENT);
		Symbol instr = take(lx, ctx.syms);

		expect(lx, is(SymbolKind::IDENTIFIER), ErrorKind::EXPECT_IDENTIFIER);
		Symbol ident = take(lx, ctx.syms);

		std::vector<Symbol> tree;
		tree.emplace_back(ident.sv, SymbolKind::INSTRUMENT);

		// TODO: Read multiple identifier-number pairs.
		// TODO: Read either identifier or MIDI here.

		// TODO: Parse string or identifier here for device name.
		expect(lx, is(SymbolKind::STRING), ErrorKind::EXPECT_STRING);
		Symbol str = take(lx, ctx.syms);
		tree.push_back(str);

		// TODO: Expect either number or identifier here.
		expect(lx, is(SymbolKind::INTEGER), ErrorKind::EXPECT_INTEGER);
		Symbol integer = take(lx, ctx.syms);
		tree.push_back(integer);

		tree.emplace_back(lx.prev.sv, SymbolKind::END);

		return tree;
	}

	inline std::vector<Symbol> let(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::INF);

		expect(lx, is(SymbolKind::LET), ErrorKind::EXPECT_LET);
		Symbol let = take(lx, ctx.syms);

		expect(lx, is(SymbolKind::IDENTIFIER), ErrorKind::EXPECT_IDENTIFIER);
		Symbol ident = take(lx, ctx.syms);

		std::vector<Symbol> tree;
		tree.emplace_back(ident.sv, SymbolKind::LET);

		// TODO: Read multiple identifier-literal pairs.
		// Let foo
		//   123
		// End
		// Let bar
		//   456
		// End

		expect(lx, is_literal, ErrorKind::EXPECT_LITERAL);
		Symbol literal = take(lx, ctx.syms);
		tree.push_back(literal);

		tree.emplace_back(lx.prev.sv, SymbolKind::END);

		return tree;
	}


	inline std::vector<Symbol> action(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::INF);

		// TODO: Expect MIDI here aswell as identifier
		expect(lx, is(SymbolKind::IDENTIFIER), ErrorKind::EXPECT_IDENTIFIER);
		Symbol ident = take(lx, ctx.syms);

		std::vector<Symbol> tree;
		tree.emplace_back(ident.sv, SymbolKind::ACTION);

		// TODO: Read multiple identifier-command pairs.
		// Action Foo
		//   Velocity
		//     127
		//   End
		// End
		// Action foo
		//   Euclide
		//     4
		//     16
		//   End
		// End
		std::vector<Symbol> cmd = command(ctx, lx);
		tree.insert(tree.end(), cmd.begin(), cmd.end());

		tree.emplace_back(lx.prev.sv, SymbolKind::END);

		return tree;
	}

	inline std::vector<Symbol> command(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::INF);

		expect(lx, is_command, ErrorKind::EXPECT_COMMAND);
		Symbol command = take(lx, ctx.syms);

		std::vector<Symbol> tree;
		tree.push_back(command);

		switch (command.kind) {
			case SymbolKind::LEFT:  // Integer argument
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME: {
				expect(lx, is(SymbolKind::INTEGER), ErrorKind::EXPECT_INTEGER);
				tree.push_back(take(lx, ctx.syms));
			} break;

			default: break;
		}

		tree.emplace_back(lx.prev.sv, SymbolKind::END);

		return tree;
	}

	inline Symbol literal(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::INF);
		expect(lx, is_literal, ErrorKind::EXPECT_LITERAL);
		return take(lx, ctx.syms);
	}
}

#endif
