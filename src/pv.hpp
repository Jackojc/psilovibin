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

// Symbol Table
namespace pv {
	struct Symbol;
	using Tree = std::vector<Symbol>;
	using SymbolTable = std::unordered_map<View, Tree>;  // TODO: Use a prefix/radix tree
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
	};

	[[nodiscard]] inline Symbol take(Lexer& lx, const SymbolTable& syms) {
		View ws = take_while(lx.sv, is_whitespace);
		Symbol sym { peek(lx.sv), SymbolKind::NONE };

		if (empty(lx.sv))  // EOF
			sym.kind = SymbolKind::TERM;

		else if (sym.sv == ","_sv) { sym.kind = SymbolKind::COMMA; lx.sv = next(lx.sv); }

		else if (sym.sv == "["_sv) { sym.kind = SymbolKind::SEQUENCE; lx.sv = next(lx.sv); }
		else if (sym.sv == "]"_sv) { sym.kind = SymbolKind::SEQUENCE_END; lx.sv = next(lx.sv); }

		else if (sym.sv == "("_sv) { sym.kind = SymbolKind::PARALLEL; lx.sv = next(lx.sv); }
		else if (sym.sv == ")"_sv) { sym.kind = SymbolKind::PARALLEL_END; lx.sv = next(lx.sv); }

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

		Symbol out = lx.peek;

		lx.prev = lx.peek;
		lx.peek = sym;

		PV_LOG(LogLevel::INF, "current: ", out);
		PV_LOG(LogLevel::INF, "peek: ", sym);

		return out;
	}

	template <typename... Ts>
	constexpr decltype(auto) is(Ts&&... kinds) {
		return [=] (Symbol other) { return ((other.kind == kinds) or ...); };
	}

	template <typename F> constexpr void expect(Lexer& lx, F&& fn, ErrorKind x) {
		if (not fn(lx.peek))
			report(lx.peek.sv, x);
	}
}

// Parser
namespace pv {
	// Store global information about program.
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

	// Utilities
	template <typename F>
	inline Tree block(View sv, SymbolKind kind, F&& fn) {
		Tree tree;
		tree.emplace_back(sv, kind);

		Tree inner_tree = fn();
		tree.insert(tree.end(), inner_tree.begin(), inner_tree.end());

		tree.emplace_back(sv, SymbolKind::END);
		return tree;
	}

	template <typename F>
	inline Tree block(Symbol sym, F&& fn) {
		return block(sym.sv, sym.kind, std::forward<F>(fn));
	}

	inline Tree cat(Tree lhs, Tree rhs) {
		lhs.insert(lhs.end(), rhs.begin(), rhs.end());
		return lhs;
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

		expect(lx, is_expr, ErrorKind::EXPECT_EXPRESSION);
		Tree expr = expression(ctx, lx);

		tree.insert(tree.end(), expr.begin(), expr.end());

		while (lx.peek.kind == SymbolKind::COMMA) {
			Symbol comma = take(lx, ctx.syms);

			expect(lx, is_expr, ErrorKind::EXPECT_EXPRESSION);
			Tree expr = expression(ctx, lx);

			tree.insert(tree.end(), expr.begin(), expr.end());
		}

		expect(lx, is(SymbolKind::TERM), ErrorKind::UNEXPECTED_TOKEN);

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

				tree.emplace_back(lx.peek.sv, SymbolKind::SELECT);
				tree.emplace_back(lx.peek.sv, SymbolKind::NONE);
				tree.emplace_back(lx.peek.sv, SymbolKind::END);

				Symbol command = take(lx, ctx.syms);

				tree.push_back(command);
				tree.emplace_back(lx.peek.sv, SymbolKind::NONE);
				tree.emplace_back(lx.prev.sv, SymbolKind::END);

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

		expect(lx, is(SymbolKind::LET), ErrorKind::EXPECT_LET);
		Symbol let = take(lx, ctx.syms);

		Tree tree;

		do {
			expect(lx, is(SymbolKind::IDENTIFIER), ErrorKind::EXPECT_IDENTIFIER);
			Symbol ident = take(lx, ctx.syms);

			tree.emplace_back(ident.sv, SymbolKind::LET);
			tree = cat(tree, literal(ctx, lx));
			tree.emplace_back(lx.prev.sv, SymbolKind::END);
		} while (lx.peek.kind == SymbolKind::IDENTIFIER);

		return tree;
	}


	inline Tree action(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		Tree tree;
		tree.emplace_back(lx.peek.sv, SymbolKind::SELECT);

		if (lx.peek.kind == SymbolKind::MIDI) {
			tree = cat(tree, midi(ctx, lx));
		}

		else if (lx.peek.kind == SymbolKind::IDENTIFIER) {
			Symbol ident = take(lx, ctx.syms);
			tree.push_back(ident);
		}

		else {
			report(lx.peek.sv, ErrorKind::EXPECT_MIDI_IDENT);
		}

		tree.emplace_back(lx.peek.sv, SymbolKind::END);

		expect(lx, is_command, ErrorKind::EXPECT_COMMAND);
		while (is_command(lx.peek))
			tree = cat(tree, command(ctx, lx));

		return tree;
	}

	inline Tree command(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		expect(lx, is_command, ErrorKind::EXPECT_COMMAND);
		Tree tree;

		switch (lx.peek.kind) {
			case SymbolKind::LEFT:  // Integer argument
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME:
			case SymbolKind::UP:
			case SymbolKind::DOWN: {
				Symbol command = take(lx, ctx.syms);

				tree.push_back(command);
				tree = cat(tree, literal(ctx, lx));
				tree.emplace_back(lx.prev.sv, SymbolKind::END);
			} break;

			case SymbolKind::GO:  // No arguments
			case SymbolKind::STOP:
			case SymbolKind::CLEAR: {
				Symbol command = take(lx, ctx.syms);

				tree.push_back(command);
				tree.emplace_back(lx.peek.sv, SymbolKind::NONE);
				tree.emplace_back(lx.prev.sv, SymbolKind::END);
			} break;

			case SymbolKind::SEQUENCE:
			case SymbolKind::PARALLEL: {
				tree = cat(tree, pattern(ctx, lx));
			} break;

			default: break;
		}

		return tree;
	}

	inline Tree pattern(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		Tree tree;

		expect(lx, is(SymbolKind::SEQUENCE, SymbolKind::PARALLEL), ErrorKind::EXPECT_PATTERN);
		Symbol open = take(lx, ctx.syms);

		tree.push_back(open);

		while (cmp_any(lx.peek.kind, SymbolKind::SEQUENCE, SymbolKind::PARALLEL) or is_literal(lx.peek)) {
			if (cmp_any(lx.peek.kind, SymbolKind::SEQUENCE, SymbolKind::PARALLEL))
				tree = cat(tree, pattern(ctx, lx));

			else if (is_literal(lx.peek))
				tree = cat(tree, literal(ctx, lx));

			else
				report(lx.peek.sv, ErrorKind::EXPECT_PATTERN);
		}

		expect(lx, is(SymbolKind::SEQUENCE_END, SymbolKind::PARALLEL_END), ErrorKind::EXPECT_PATTERN_END);
		Symbol close = take(lx, ctx.syms);

		tree.emplace_back(close.sv, SymbolKind::END);

		return tree;
	}

	inline Tree midi(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		Tree tree;

		expect(lx, is(SymbolKind::MIDI), ErrorKind::EXPECT_MIDI);
		Symbol midi = take(lx, ctx.syms);

		tree.push_back(midi);

		tree = cat(tree, literal(ctx, lx));
		tree = cat(tree, literal(ctx, lx));

		tree.emplace_back(lx.peek.sv, SymbolKind::END);

		return tree;
	}

	inline Tree literal(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		Tree tree;

		expect(lx, is_literal, ErrorKind::EXPECT_LITERAL);
		switch (lx.peek.kind) {
			case SymbolKind::INTEGER:
			case SymbolKind::IDENTIFIER:
			case SymbolKind::STRING: {
				Symbol lit = take(lx, ctx.syms);
				tree.push_back(lit);
			} break;

			case SymbolKind::MIDI: {
				tree = cat(tree, midi(ctx, lx));
			} break;

			default: break;
		}

		return tree;
	}

	template <typename F, typename... Ts>
	inline Tree visitor(
		const F& callback,
		Context& ctx,
		const Tree& tree,
		Tree::const_iterator& it,
		Ts&&... args
	) {
		if (it == tree.cend())
			return {};

		Tree::const_iterator current = it++;
		return callback(ctx, tree, current, it, std::forward<Ts>(args)...);
	}

	// Visit a block (i.e. a run of code terminated by `end`).
	// This is a common pattern that shows up in most visitors that
	// traverse the AST like a tree rather than a vector.
	template <typename F, typename... Ts>
	inline Tree visit_block_inclusive(
		F&& fn,
		Context& ctx,
		const Tree& tree,
		Tree::const_iterator& it,
		Ts&&... args
	) {
		Tree new_tree;

		Tree::const_iterator before = it;

		while (it != tree.cend() and cmp_none(it->kind, SymbolKind::END, SymbolKind::TERM)) {
			new_tree = cat(new_tree, visitor(fn, ctx, tree, it, std::forward<Ts>(args)...));
		}

		if (it->kind != SymbolKind::END)
			report(before->sv, ErrorKind::INVALID_AST);

		return new_tree;
	}

	template <typename F, typename... Ts>
	inline Tree visit_block(
		F&& fn,
		Context& ctx,
		const Tree& tree,
		Tree::const_iterator& it,
		Ts&&... args
	) {
		Tree new_tree = visit_block_inclusive(std::forward<F>(fn), ctx, tree, it, std::forward<Ts>(args)...);
		++it;
		return new_tree;
	}

	// Runs a pass by visiting every top level node until EOF is reached.
	template <typename F, typename... Ts>
	inline Tree pass(
		F&& fn,
		Context& ctx,
		const Tree& tree,
		Ts&&... args
	) {
		Tree new_tree;
		Tree::const_iterator it = tree.cbegin();

		while (it != tree.cend() and it->kind != SymbolKind::TERM) {
			new_tree = cat(new_tree, visitor(fn, ctx, tree, it, std::forward<Ts>(args)...));
		}

		return new_tree;
	}

	// template <typename F, typename... Ts>
	// inline std::pair<Tree::const_iterator, Tree::const_iterator> visit_block_extent_inclusive(
	// 	F&& fn,
	// 	Context& ctx,
	// 	const Tree& tree,
	// 	Tree::const_iterator& it,
	// 	Ts&&... args
	// ) {
	// 	Tree::const_iterator before = it;
	// 	Tree::const_iterator after = visit_block_inclusive(fn, ctx, tree, it, std::forward<Ts>(args)...);

	// 	return { before, after };
	// }

	// template <typename F, typename... Ts>
	// inline std::pair<Tree::const_iterator, Tree::const_iterator> visit_block_extent(
	// 	F&& fn,
	// 	Context& ctx,
	// 	const Tree& tree,
	// 	Tree::const_iterator it,
	// 	Ts&&... args
	// ) {
	// 	auto [before, after] = visit_block_extent(std::forward<F>(fn), ctx, tree, it, std::forward<Ts>(args)...);
	// 	return { before, after + 1 };
	// }

	namespace detail {
		// TODO: Allow for passing a function that asserts some invariances about the
		// node. For example we may want to check the kind of a node _and_ the value
		// it contains.
		template <typename X, typename Y>
		inline Tree::const_iterator match_impl(
			const Tree& tree,
			Tree::const_iterator it,
			X&& kind,
			Y&& err
		) {
			bool is_matching = it != tree.cend() and it->kind == kind;

			if (not is_matching)
				report(it->sv, err);

			return ++it;
		}
	}

	template <typename X, typename Y, typename... Ts>
	inline Tree::const_iterator match(
		const Tree& tree,
		Tree::const_iterator it,
		X&& kind,
		Y&& err,
		Ts&&... pairs
	) {
		static_assert(sizeof...(Ts) % 2 == 0);

		it = detail::match_impl(tree, it, std::forward<X>(kind), std::forward<Y>(err));

		if constexpr(sizeof...(Ts) >= 2)
			it = match(tree, it, std::forward<Ts>(pairs)...);

		return it;
	}
}

#endif
