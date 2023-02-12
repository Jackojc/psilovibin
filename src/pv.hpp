#ifndef PV_HPP
#define PV_HPP

/*
	HEADERS

	#include <type_traits>
	#include <algorithm>
	#include <array>
	#include <vector>
	#include <unordered_set>
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
		X(EXPECT_COMMAND,    "expected a command") \
		X(EXPECT_EXPRESSION, "expected an expression") \
		X(EXPECT_INTEGER,    "expected an integer") \
		X(EXPECT_STRING,     "expected a string") \
		X(EXPECT_LITERAL,    "expected a literal") \
		X(EXPECT_MIDI,       "expected a device") \
		X(EXPECT_LET,        "expected let") \
		X(EXPECT_IDENTIFIER, "expected an identifier") \
		X(EXPECT_CONTROL,    "expected control") \
		X(EXPECT_INSTRUMENT, "expected instrument") \
		\
		X(EXPECT_MIDI_IDENT,   "expected device or identifier") \
		X(EXPECT_MIDI_LITERAL, "expected device or literal") \
		X(EXPECT_NUMBER_IDENT, "expected number or identifier")

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

	template <typename... Ts>
	[[noreturn]] constexpr void report(View sv, ErrorKind x) {
		throw Report { sv, x };
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
		X(SELECT, "Select") \
		X(END,    "End") \
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
		\
		X(MIDI,       "Midi") \
		X(LET,        "Let") \
		X(INSTRUMENT, "Instrument") \
		X(CONTROL,    "Control") \
		X(ACTION,     "Action")

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
			else if (sym.sv == "clear"_sv)    sym.kind = SymbolKind::CLEAR;
			else if (sym.sv == "info"_sv)     sym.kind = SymbolKind::INFO;

			else if (sym.sv == "midi"_sv)  sym.kind = SymbolKind::MIDI;
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

		PV_LOG(LogLevel::INF, out);

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
			SymbolKind::STRING);
	}

	constexpr bool is_midi_or_ident(Symbol x) {
		return cmp_any(x.kind,
			SymbolKind::MIDI,
			SymbolKind::IDENTIFIER);
	}

	constexpr bool is_midi_or_literal(Symbol x) {
		return is_literal(x) or cmp_any(x.kind,
			SymbolKind::MIDI);
	}

	constexpr bool is_number_or_ident(Symbol x) {
		return cmp_any(x.kind,
			SymbolKind::INTEGER,
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
			SymbolKind::INFO);
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
			SymbolKind::IDENTIFIER,
			SymbolKind::CONTROL,
			SymbolKind::INSTRUMENT,
			SymbolKind::LET,
			SymbolKind::MIDI);
	}

	// Utilities
	template <typename F>
	inline std::vector<Symbol> block(View sv, SymbolKind kind, F&& fn) {
		std::vector<Symbol> tree;
		tree.emplace_back(sv, kind);

		std::vector<Symbol> inner_tree = fn();
		tree.insert(tree.end(), inner_tree.begin(), inner_tree.end());

		tree.emplace_back(sv, SymbolKind::END);
		return tree;
	}

	template <typename F>
	inline std::vector<Symbol> block(Symbol sym, F&& fn) {
		return block(sym.sv, sym.kind, std::forward<F>(fn));
	}

	inline std::vector<Symbol> cat(std::vector<Symbol> lhs, std::vector<Symbol> rhs) {
		lhs.insert(lhs.end(), rhs.begin(), rhs.end());
		return lhs;
	}

	// Parser functions.
	inline std::vector<Symbol> program(Context&, Lexer&);
	inline std::vector<Symbol> expression(Context&, Lexer&);

	inline std::vector<Symbol> control(Context&, Lexer&);
	inline std::vector<Symbol> instrument(Context&, Lexer&);
	inline std::vector<Symbol> let(Context&, Lexer&);

	inline std::vector<Symbol> action(Context&, Lexer&);
	inline std::vector<Symbol> command(Context&, Lexer&);

	inline std::vector<Symbol> midi(Context&, Lexer&);
	inline std::vector<Symbol> literal(Context&, Lexer&);


	inline std::vector<Symbol> program(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		std::vector<Symbol> tree;

		expect(lx, is_expr, ErrorKind::EXPECT_EXPRESSION);
		std::vector<Symbol> expr = expression(ctx, lx);

		tree.insert(tree.end(), expr.begin(), expr.end());

		while (lx.peek.kind == SymbolKind::COMMA) {
			Symbol comma = take(lx, ctx.syms);

			expect(lx, is_expr, ErrorKind::EXPECT_EXPRESSION);
			std::vector<Symbol> expr = expression(ctx, lx);

			tree.insert(tree.end(), expr.begin(), expr.end());
		}

		expect(lx, is(SymbolKind::TERM), ErrorKind::UNEXPECTED_TOKEN);

		return tree;
	}

	inline std::vector<Symbol> expression(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		switch (lx.peek.kind) {
			case SymbolKind::LET:        return let(ctx, lx);  // Statements
			case SymbolKind::INSTRUMENT: return instrument(ctx, lx);
			case SymbolKind::CONTROL:    return control(ctx, lx);

			case SymbolKind::GO:  // Top-level commands
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				std::vector<Symbol> tree;

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


	inline std::vector<Symbol> control(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		expect(lx, is(SymbolKind::CONTROL), ErrorKind::EXPECT_CONTROL);
		Symbol ctrl = take(lx, ctx.syms);

		std::vector<Symbol> tree;

		do {
			expect(lx, is(SymbolKind::IDENTIFIER), ErrorKind::EXPECT_IDENTIFIER);
			Symbol ident = take(lx, ctx.syms);

			tree.emplace_back(ident.sv, SymbolKind::CONTROL);

			expect(lx, is_number_or_ident, ErrorKind::EXPECT_NUMBER_IDENT);

			if (lx.peek.kind == SymbolKind::INTEGER) {
				Symbol integer = take(lx, ctx.syms);
				tree.push_back(integer);
			}

			else if (lx.peek.kind == SymbolKind::IDENTIFIER) {
				Symbol ident = take(lx, ctx.syms);
				tree.push_back(ident);
			}

			tree.emplace_back(lx.prev.sv, SymbolKind::END);
		} while (lx.peek.kind == SymbolKind::IDENTIFIER);

		return tree;
	}

	inline std::vector<Symbol> instrument(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		expect(lx, is(SymbolKind::INSTRUMENT), ErrorKind::EXPECT_INSTRUMENT);
		Symbol instr = take(lx, ctx.syms);

		std::vector<Symbol> tree;

		do {
			expect(lx, is(SymbolKind::IDENTIFIER), ErrorKind::EXPECT_IDENTIFIER);
			Symbol ident = take(lx, ctx.syms);

			tree.emplace_back(ident.sv, SymbolKind::INSTRUMENT);

			expect(lx, is_midi_or_ident, ErrorKind::EXPECT_MIDI_IDENT);

			if (lx.peek.kind == SymbolKind::MIDI) {
				std::vector<Symbol> dev = midi(ctx, lx);
				tree.insert(tree.end(), dev.begin(), dev.end());
			}

			else if (lx.peek.kind == SymbolKind::IDENTIFIER) {
				Symbol ident = take(lx, ctx.syms);
				tree.push_back(ident);
			}

			tree.emplace_back(lx.prev.sv, SymbolKind::END);
		} while (lx.peek.kind == SymbolKind::IDENTIFIER);

		return tree;
	}

	inline std::vector<Symbol> let(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		expect(lx, is(SymbolKind::LET), ErrorKind::EXPECT_LET);
		Symbol let = take(lx, ctx.syms);

		std::vector<Symbol> tree;

		do {
			expect(lx, is(SymbolKind::IDENTIFIER), ErrorKind::EXPECT_IDENTIFIER);
			Symbol ident = take(lx, ctx.syms);

			tree.emplace_back(ident.sv, SymbolKind::LET);

			expect(lx, is_midi_or_literal, ErrorKind::EXPECT_MIDI_LITERAL);
			if (lx.peek.kind == SymbolKind::MIDI)
				tree = cat(tree, midi(ctx, lx));

			else if (is_literal(lx.peek))
				tree = cat(tree, literal(ctx, lx));

			tree.emplace_back(lx.prev.sv, SymbolKind::END);
		} while (lx.peek.kind == SymbolKind::IDENTIFIER);

		return tree;
	}


	inline std::vector<Symbol> action(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		std::vector<Symbol> tree;
		tree.emplace_back(lx.peek.sv, SymbolKind::SELECT);

		expect(lx, is_midi_or_ident, ErrorKind::EXPECT_MIDI_IDENT);
		if (lx.peek.kind == SymbolKind::MIDI) {
			tree = cat(tree, midi(ctx, lx));
		}

		else if (lx.peek.kind == SymbolKind::IDENTIFIER) {
			Symbol ident = take(lx, ctx.syms);
			tree.push_back(ident);
		}

		tree.emplace_back(lx.peek.sv, SymbolKind::END);

		expect(lx, is_command, ErrorKind::EXPECT_COMMAND);
		while (is_command(lx.peek))
			tree = cat(tree, command(ctx, lx));

		return tree;
	}

	inline std::vector<Symbol> command(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		expect(lx, is_command, ErrorKind::EXPECT_COMMAND);
		std::vector<Symbol> tree;

		Symbol command = take(lx, ctx.syms);
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

			case SymbolKind::GO:  // No arguments
			case SymbolKind::STOP:
			case SymbolKind::CLEAR: {
				tree.emplace_back(lx.peek.sv, SymbolKind::NONE);
			} break;

			default: break;
		}

		tree.emplace_back(lx.prev.sv, SymbolKind::END);

		return tree;
	}

	inline std::vector<Symbol> midi(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		std::vector<Symbol> tree;

		expect(lx, is(SymbolKind::MIDI), ErrorKind::EXPECT_MIDI);
		Symbol midi = take(lx, ctx.syms);

		expect(lx, is(SymbolKind::STRING), ErrorKind::EXPECT_STRING);
		Symbol str = take(lx, ctx.syms);

		tree.emplace_back(str.sv, SymbolKind::MIDI);
		tree = cat(tree, literal(ctx, lx));
		tree.emplace_back(lx.peek.sv, SymbolKind::END);

		return tree;
	}

	inline std::vector<Symbol> literal(Context& ctx, Lexer& lx) {
		PV_LOG(LogLevel::WRN);

		expect(lx, is_literal, ErrorKind::EXPECT_LITERAL);
		Symbol lit = take(lx, ctx.syms);

		return { lit };
	}

	template <typename F, typename... Ts>
	inline std::vector<Symbol>::iterator visitor(
		const F& callback,
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator it,
		Ts&&... args
	) {
		if (it == tree.end())
			return it;

		auto [sv, kind] = *it++;
		bool matched = callback(ctx, tree, sv, kind, it, std::forward<Ts>(args)...);

		if (not matched)
			PV_LOG(LogLevel::WRN, "unhandled symbol: `", kind, "`");

		return it;
	}

	// Visit a block (i.e. a run of code terminated by `end`).
	// This is a common pattern that shows up in most visitors that
	// traverse the AST like a tree rather than a vector.
	template <typename F, typename... Ts>
	inline std::vector<Symbol>::iterator visit_block(
		F&& fn,
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator it,
		Ts&&... args
	) {
		std::vector<Symbol>::iterator before = it;

		while (it != tree.end() and cmp_none(it->kind, SymbolKind::END, SymbolKind::TERM))
			it = visitor(fn, ctx, tree, it, std::forward<Ts>(args)...);

		if (it->kind != SymbolKind::END)
			report(before->sv, ErrorKind::INVALID_AST);

		return it + 1;
	}

	// Runs a pass by visiting every top level node until EOF is reached.
	template <typename F, typename... Ts>
	inline std::vector<Symbol>::iterator pass(
		F&& fn,
		Context& ctx,
		std::vector<Symbol>& tree,
		Ts&&... args
	) {
		std::vector<Symbol>::iterator it = tree.begin();
		std::vector<Symbol>::iterator before = it;

		while (it != tree.end() and it->kind != SymbolKind::TERM)
			it = visitor(fn, ctx, tree, it, std::forward<Ts>(args)...);

		return it;
	}
}

#endif
