#ifndef PV_HPP
#define PV_HPP

/*
	HEADERS

	#include <type_traits>
	#include <algorithm>
	#include <array>
	#include <vector>
	#include <unordered_map>
	#include <string_view>
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
		X(INVALID_AST,      "invalid tree") \
		\
		X(EXPECT_COMMAND,     "expected a command") \
		X(EXPECT_EXPRESSION,  "expected an expression") \
		X(EXPECT_INTEGER,     "expected an integer") \
		X(EXPECT_STRING,      "expected a string") \
		X(EXPECT_LITERAL,     "expected a literal") \
		X(EXPECT_MIDI,        "expected a device") \
		X(EXPECT_LET,         "expected let") \
		X(EXPECT_IDENTIFIER,  "expected an identifier") \
		X(EXPECT_MIDI_IDENT,  "expected device or identifier") \
		X(EXPECT_PATTERN,     "expected a pattern") \
		X(EXPECT_PATTERN_END, "expected end of pattern") \
		\
		X(UNKNOWN_SYMBOL, "unknown symbol")

	#define X(a, b) a,
		enum class ErrorKind: size_t { ERROR_KINDS };
	#undef X

	namespace detail {
		#define X(a, b) std::string_view { b },
			constexpr std::array ERROR_TO_STR = { ERROR_KINDS };
		#undef X

		constexpr std::string_view error_to_str(ErrorKind x) {
			return ERROR_TO_STR[static_cast<size_t>(x)];
		}

		constexpr size_t error_padding(size_t n) {
			constexpr size_t max = std::max_element(detail::ERROR_TO_STR.begin(), detail::ERROR_TO_STR.end())->size();
			return n > max ? max : max - n;
		}
	}

	inline std::ostream& operator<<(std::ostream& os, ErrorKind x) {
		return print(os, detail::error_to_str(x));
	}

	struct Report {
		View sv;
		ErrorKind kind;
	};

	[[noreturn]] inline void report(View sv, ErrorKind x) {
		throw Report { sv, x };
	}

	[[noreturn]] inline void report(ErrorKind x) {
		throw Report { ""_sv, x };
	}

	inline std::ostream& report_handler(std::ostream& os, Report x) {
		return empty(x.sv) ?
			println(os, "[" PV_ERR "fail" PV_RESET "] ", x.kind):
			println(os, "[" PV_ERR "fail" PV_RESET "] ", x.kind, ": `", x.sv, "`");
	}

	inline std::ostream& operator<<(std::ostream& os, Report x) {
		return report_handler(os, x);
	}
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
		X(SELECT,  "Select") \
		X(END,     "End") \
		X(NOTE,    "Note") \
		X(NEST,    "Nest") \
		X(DESCEND, "Descend") \
		X(ASCEND,  "Ascend") \
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
		X(CLEAR,    "Clear") \
		X(INFO,     "Info") \
		X(UP,       "Up") \
		X(DOWN,     "Down") \
		\
		X(SEQUENCE,     "Sequence") \
		X(SEQUENCE_END, "Sequence End") \
		X(PARALLEL,     "Parallel") \
		X(PARALLEL_END, "Parallel End") \
		\
		X(MIDI,    "Midi") \
		X(LET,     "Let")

	#define X(a, b) a,
		enum class SymbolKind: size_t { SYMBOL_KINDS };
	#undef X

	namespace detail {
		#define X(a, b) std::string_view { b },
			constexpr std::array SYMBOL_TO_STR = { SYMBOL_KINDS };
		#undef X

		constexpr std::string_view symbol_to_str(SymbolKind x) {
			return detail::SYMBOL_TO_STR[static_cast<size_t>(x)];
		}

		constexpr size_t symbol_padding(size_t n = 0) {
			constexpr size_t max = std::max_element(detail::SYMBOL_TO_STR.begin(), detail::SYMBOL_TO_STR.end())->size();
			return n > max ? max : max - n;
		}
	}

	inline std::ostream& operator<<(std::ostream& os, SymbolKind x) {
		return print(os, detail::symbol_to_str(x));
	}

	struct Symbol {
		View sv;
		SymbolKind kind;

		constexpr Symbol():
			sv(""_sv), kind(SymbolKind::NONE) {}

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

		template <typename F> constexpr void expect(F&& fn, ErrorKind x) {
			if (not fn(peek))
				report(peek.sv, x);
		}

		[[nodiscard]] inline Symbol take() {
			View ws = take_while(sv, is_whitespace);
			Symbol sym { pv::peek(sv), SymbolKind::NONE };

			if (empty(sv))  // EOF
				sym.kind = SymbolKind::TERM;

			else if (sym.sv == ","_sv) { sym.kind = SymbolKind::COMMA; sv = next(sv); }

			else if (sym.sv == "["_sv) { sym.kind = SymbolKind::SEQUENCE; sv = next(sv); }
			else if (sym.sv == "]"_sv) { sym.kind = SymbolKind::SEQUENCE_END; sv = next(sv); }

			else if (sym.sv == "("_sv) { sym.kind = SymbolKind::PARALLEL; sv = next(sv); }
			else if (sym.sv == ")"_sv) { sym.kind = SymbolKind::PARALLEL_END; sv = next(sv); }

			else if (sym.sv == "\""_sv) {
				sym.kind = SymbolKind::STRING;
				sv = next(sv);

				sym.sv = take_while(sv, [] (View sv) {
					return sv != "\"";
				});

				sv = next(sv);
			}

			else if (is_digit(sym.sv)) {
				sym.kind = SymbolKind::INTEGER;
				sym.sv = take_while(sv, is_digit);
			}

			else if (is_alpha(sym.sv)) {  // Identifiers.
				sym.kind = SymbolKind::IDENTIFIER;
				sym.sv = take_while(sv, is_alphanumeric);

				if (begins_with(sym.sv, "#"_sv)) {  // Comments.
					View comment = take_while(sv, [] (View sv) {
						return sv != "\n"_sv;
					});

					sv = next(sv);
					return take();
				}

				// TODO: Use incremental matching for keywords/symbols.

				if      (sym.sv == "go"_sv)       sym.kind = SymbolKind::GO;
				else if (sym.sv == "stop"_sv)     sym.kind = SymbolKind::STOP;
				else if (sym.sv == "left"_sv)     sym.kind = SymbolKind::LEFT;
				else if (sym.sv == "right"_sv)    sym.kind = SymbolKind::RIGHT;
				else if (sym.sv == "velocity"_sv) sym.kind = SymbolKind::VELOCITY;
				else if (sym.sv == "bpm"_sv)      sym.kind = SymbolKind::BPM;
				else if (sym.sv == "time"_sv)     sym.kind = SymbolKind::TIME;
				else if (sym.sv == "clear"_sv)    sym.kind = SymbolKind::CLEAR;
				else if (sym.sv == "info"_sv)     sym.kind = SymbolKind::INFO;
				else if (sym.sv == "up"_sv)       sym.kind = SymbolKind::UP;
				else if (sym.sv == "down"_sv)     sym.kind = SymbolKind::DOWN;

				else if (sym.sv == "midi"_sv)  sym.kind = SymbolKind::MIDI;
				else if (sym.sv == "let"_sv)   sym.kind = SymbolKind::LET;
			}

			else
				// TODO: This fails and prints nothing (visibly) if character is UTF-8.
				report(sym.sv, ErrorKind::UNKNOWN_CHAR);

			Symbol out = peek;

			prev = peek;
			peek = sym;

			PV_LOG(LogLevel::INF, "current: ", out);
			PV_LOG(LogLevel::INF, "peek: ", sym);

			return out;
		}
	};

	template <typename... Ts>
	constexpr decltype(auto) is(Ts&&... kinds) {
		return [=] (Symbol other) { return ((other.kind == kinds) or ...); };
	}
}

// Parser
namespace pv {
	struct Tree: public std::vector<Symbol> {
		using std::vector<Symbol>::vector;

		Tree cat(Tree other) {
			insert(end(), other.begin(), other.end());
			return *this;
		}
	};

	inline std::ostream& operator<<(std::ostream& os, const Tree& t) {
		os << '[';

		if (not t.empty()) {
			auto it = t.begin();

			os << *it;

			for (++it; it != t.end(); ++it)
				os << ", " << *it;
		}

		return (os << ']');
	}

	// Store global information about program.
	using SymbolTable = std::unordered_map<View, Tree>;

	struct Context {
		SymbolTable syms;
	};

	// Categories.
	constexpr bool is_literal(Symbol x) {
		return cmp_any(x.kind,
			SymbolKind::INTEGER,
			SymbolKind::STRING,
			SymbolKind::MIDI,
			SymbolKind::IDENTIFIER);
	}

	constexpr bool is_command(Symbol x) {
		return cmp_any(x.kind,
			SymbolKind::GO,
			SymbolKind::STOP,
			SymbolKind::LEFT,
			SymbolKind::RIGHT,
			SymbolKind::VELOCITY,
			SymbolKind::BPM,
			SymbolKind::TIME,
			SymbolKind::CLEAR,
			SymbolKind::INFO,
			SymbolKind::UP,
			SymbolKind::DOWN,
			SymbolKind::SEQUENCE,
			SymbolKind::PARALLEL);
	}

	constexpr bool is_tl_command(Symbol x) {
		return cmp_any(x.kind,
			SymbolKind::GO,
			SymbolKind::STOP,
			SymbolKind::CLEAR,
			SymbolKind::INFO);
	}

	constexpr bool is_expr(Symbol x) {
		return is_tl_command(x) or cmp_any(x.kind,
			SymbolKind::LET,
			SymbolKind::IDENTIFIER,
			SymbolKind::MIDI);
	}

	// Parser functions.
	inline Tree program(Context&, Lexer&);
	inline Tree expression(Context&, Lexer&);

	inline Tree let(Context&, Lexer&);

	inline Tree action(Context&, Lexer&);
	inline Tree command(Context&, Lexer&);

	inline Tree pattern(Context&, Lexer&);

	inline Tree midi(Context&, Lexer&);
	inline Tree literal(Context&, Lexer&);


	inline Tree program(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		Tree tree;

		lx.expect(is_expr, ErrorKind::EXPECT_EXPRESSION);
		Tree expr = expression(ctx, lx);

		tree.insert(tree.end(), expr.begin(), expr.end());

		while (lx.peek.kind == SymbolKind::COMMA) {
			Symbol comma = lx.take();

			lx.expect(is_expr, ErrorKind::EXPECT_EXPRESSION);
			Tree expr = expression(ctx, lx);

			tree.insert(tree.end(), expr.begin(), expr.end());
		}

		lx.expect(is(SymbolKind::TERM), ErrorKind::UNEXPECTED_TOKEN);

		return tree;
	}

	inline Tree expression(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		switch (lx.peek.kind) {
			case SymbolKind::LET: return let(ctx, lx);  // Statements

			case SymbolKind::GO:  // Top-level commands
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				Tree tree;

				tree.emplace_back(lx.peek.sv, SymbolKind::NONE);
				tree.emplace_back(lx.peek.sv, SymbolKind::SELECT);

				Symbol command = lx.take();

				tree.emplace_back(lx.peek.sv, SymbolKind::NONE);
				tree.push_back(command);

				return tree;
			} break;

			case SymbolKind::IDENTIFIER:
			case SymbolKind::MIDI: {
				return action(ctx, lx);  // Action
			} break;

			default: {
				report(lx.peek.sv, ErrorKind::EXPECT_EXPRESSION);
			} break;
		}

		report(lx.peek.sv, ErrorKind::UNREACHABLE);
		return {};
	}

	inline Tree let(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		lx.expect(is(SymbolKind::LET), ErrorKind::EXPECT_LET);
		Symbol let = lx.take();

		Tree tree;

		do {
			lx.expect(is(SymbolKind::IDENTIFIER), ErrorKind::EXPECT_IDENTIFIER);
			Symbol ident = lx.take();

			tree.cat(literal(ctx, lx));
			tree.emplace_back(ident.sv, SymbolKind::LET);
		} while (lx.peek.kind == SymbolKind::IDENTIFIER);

		return tree;
	}


	inline Tree action(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		Tree tree;

		if (lx.peek.kind == SymbolKind::MIDI)
			tree.cat(midi(ctx, lx));

		else if (lx.peek.kind == SymbolKind::IDENTIFIER) {
			Symbol ident = lx.take();
			tree.push_back(ident);
		}

		else
			report(lx.peek.sv, ErrorKind::EXPECT_MIDI_IDENT);

		tree.emplace_back(lx.peek.sv, SymbolKind::SELECT);

		lx.expect(is_command, ErrorKind::EXPECT_COMMAND);
		while (is_command(lx.peek))
			tree.cat(command(ctx, lx));

		return tree;
	}

	inline Tree command(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		lx.expect(is_command, ErrorKind::EXPECT_COMMAND);
		Tree tree;

		switch (lx.peek.kind) {
			case SymbolKind::LEFT:  // Integer argument
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME:
			case SymbolKind::UP:
			case SymbolKind::DOWN: {
				Symbol command = lx.take();
				tree.cat(literal(ctx, lx));
				tree.push_back(command);
			} break;

			case SymbolKind::GO:  // No arguments
			case SymbolKind::STOP:
			case SymbolKind::CLEAR: {
				Symbol command = lx.take();

				tree.emplace_back(lx.peek.sv, SymbolKind::NONE);
				tree.push_back(command);
			} break;

			case SymbolKind::SEQUENCE:
			case SymbolKind::PARALLEL: {
				tree.cat(pattern(ctx, lx));
			} break;

			default: break;
		}

		return tree;
	}

	inline Tree pattern(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		Tree tree;

		lx.expect(is(SymbolKind::SEQUENCE, SymbolKind::PARALLEL), ErrorKind::EXPECT_PATTERN);
		Symbol open = lx.take();

		tree.push_back(open);

		while (cmp_any(lx.peek.kind, SymbolKind::SEQUENCE, SymbolKind::PARALLEL) or is_literal(lx.peek)) {
			if (cmp_any(lx.peek.kind, SymbolKind::SEQUENCE, SymbolKind::PARALLEL))
				tree.cat(pattern(ctx, lx));

			else if (is_literal(lx.peek))
				tree.cat(literal(ctx, lx));

			else
				report(lx.peek.sv, ErrorKind::EXPECT_PATTERN);
		}

		lx.expect(is(SymbolKind::SEQUENCE_END, SymbolKind::PARALLEL_END), ErrorKind::EXPECT_PATTERN_END);
		Symbol close = lx.take();

		tree.emplace_back(close.sv, SymbolKind::END);

		return tree;
	}

	inline Tree midi(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		Tree tree;

		lx.expect(is(SymbolKind::MIDI), ErrorKind::EXPECT_MIDI);
		Symbol midi = lx.take();

		tree.cat(literal(ctx, lx));
		tree.cat(literal(ctx, lx));

		tree.push_back(midi);

		return tree;
	}

	inline Tree literal(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		Tree tree;

		lx.expect(is_literal, ErrorKind::EXPECT_LITERAL);
		switch (lx.peek.kind) {
			case SymbolKind::INTEGER:
			case SymbolKind::IDENTIFIER:
			case SymbolKind::STRING: {
				Symbol lit = lx.take();
				tree.push_back(lit);
			} break;

			case SymbolKind::MIDI: {
				tree.cat(midi(ctx, lx));
			} break;

			default: break;
		}

		return tree;
	}

	// template <typename F, typename... Ts>
	// inline Tree::iterator visitor(
	// 	const F& callback,
	// 	Context& ctx,
	// 	Tree& tree,
	// 	Tree::iterator it,
	// 	Ts&&... args
	// ) {
	// 	if (it == tree.end())
	// 		return it;

	// 	Tree::iterator current = it++;
	// 	bool matched = callback(ctx, tree, current, it, std::forward<Ts>(args)...);

	// 	if (not matched)
	// 		PV_LOG(LogLevel::WRN, "unhandled symbol: `", current->kind, "`");

	// 	return it;
	// }

	// // Visit a block (i.e. a run of code terminated by `end`).
	// // This is a common pattern that shows up in most visitors that
	// // traverse the AST like a tree rather than a vector.
	// template <typename F, typename... Ts>
	// inline Tree::iterator visit_block(
	// 	F&& fn,
	// 	Context& ctx,
	// 	Tree& tree,
	// 	Tree::iterator it,
	// 	Ts&&... args
	// ) {
	// 	Tree::iterator before = it;

	// 	while (it != tree.end() and cmp_none(it->kind, SymbolKind::END, SymbolKind::TERM))
	// 		it = visitor(fn, ctx, tree, it, std::forward<Ts>(args)...);

	// 	if (it->kind != SymbolKind::END)
	// 		report(before->sv, ErrorKind::INVALID_AST);

	// 	return it + 1;
	// }

	// // Runs a pass by visiting every top level node until EOF is reached.
	// template <typename F, typename... Ts>
	// inline Tree::iterator pass(
	// 	F&& fn,
	// 	Context& ctx,
	// 	Tree& tree,
	// 	Ts&&... args
	// ) {
	// 	Tree::iterator it = tree.begin();

	// 	while (it != tree.end() and it->kind != SymbolKind::TERM)
	// 		it = visitor(fn, ctx, tree, it, std::forward<Ts>(args)...);

	// 	return it;
	// }

	// template <typename F, typename... Ts>
	// inline Tree get_block(
	// 	F&& fn,
	// 	Context& ctx,
	// 	Tree& tree,
	// 	Tree::iterator it,
	// 	Ts&&... args
	// ) {
	// 	Tree::iterator before = it;
	// 	Tree::iterator after = visit_block(fn, ctx, tree, it, std::forward<Ts>(args)...);

	// 	return { before, after - 1 };
	// }

	// namespace detail {
	// 	// TODO: Allow for passing a function that asserts some invariances about the
	// 	// node. For example we may want to check the kind of a node _and_ the value
	// 	// it contains.
	// 	template <typename X, typename Y>
	// 	inline Tree::iterator match_impl(
	// 		Tree& tree,
	// 		Tree::iterator it,
	// 		X&& kind,
	// 		Y&& err
	// 	) {
	// 		bool is_matching = it != tree.end() and it->kind == kind;

	// 		if (not is_matching)
	// 			report(it->sv, err);

	// 		return ++it;
	// 	}
	// }

	// template <typename X, typename Y, typename... Ts>
	// inline Tree::iterator match(
	// 	Tree& tree,
	// 	Tree::iterator it,
	// 	X&& kind,
	// 	Y&& err,
	// 	Ts&&... pairs
	// ) {
	// 	static_assert(sizeof...(Ts) % 2 == 0);

	// 	it = detail::match_impl(tree, it, std::forward<X>(kind), std::forward<Y>(err));

	// 	if constexpr(sizeof...(Ts) >= 2)
	// 		it = match(tree, it, std::forward<Ts>(pairs)...);

	// 	return it;
	// }
}

#endif
