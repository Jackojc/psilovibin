#ifndef PV_HPP
#define PV_HPP

/*
	HEADERS

	#include <unordered_set>
	#include <iostream>
*/

// Errors
namespace pv {
	#define ERROR_KINDS \
		X(GENERIC,            "an error occurred") \
		X(UNREACHABLE,        "unreachable code") \
		X(NOT_IMPLEMENTED,    "not implemented") \
		X(UNKNOWN_CHAR,       "unknown character") \
		X(UNDEFINED,          "undefined")

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
		\
		X(IDENT,   "Identifier") \
		X(INTEGER, "Integer") \
		X(STRING,  "String") \
		X(COMMA,   "Comma") \
		\
		X(GO,       "Go") \
		X(STOP,     "Stop") \
		X(LEFT,     "Left") \
		X(RIGHT,    "Right") \
		X(VELOCITY, "Velocity") \
		X(BPM,      "BPM") \
		X(TIME,     "Time") \

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
			sym.kind = SymbolKind::IDENT;
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
		if (not fn(lx.peek))
			report(lx.peek.sv, x);
	}
}

#endif
