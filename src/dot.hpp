#ifndef PV_PASS_DOT_HPP
#define PV_PASS_DOT_HPP

/*
	HEADERS

	#include <iostream>
	#include <util.hpp>
	#include <pv.hpp>
*/

/*
	This pass generates a dot graph (graphviz) of the AST.
*/
namespace pv {
	namespace detail {
		template <typename... Ts>
		inline std::ostream& edge(std::ostream& os, size_t from, size_t to, Ts&&... label) {
			println(os, "  n", to, " [label=\"", std::forward<Ts>(label)..., "\"];");

			if (from != to)
				println(os, "  n", from, " -> n", to, ";");

			return os;
		}
	}

	bool dot_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		View sv,
		SymbolKind kind,
		std::vector<Symbol>::iterator& it,
		size_t& id,
		size_t parent_id = 0
	) {
		size_t self_id = id++;

		switch (kind) {
			case SymbolKind::NONE: {
				detail::edge(std::cout, parent_id, self_id, kind);
			} break;

			case SymbolKind::STRING:  // Literals.
			case SymbolKind::INTEGER:
			case SymbolKind::IDENTIFIER: {
				detail::edge(std::cout, parent_id, self_id, kind, "|", sv);
			} break;

			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:

			case SymbolKind::LEFT:  // Commands with arguments.
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME:

			case SymbolKind::LET:  // Expressions/Statements.
			case SymbolKind::INSTRUMENT:
			case SymbolKind::CONTROL:
			case SymbolKind::ACTION: {
				detail::edge(std::cout, parent_id, self_id, kind, "|", sv);
				it = visit_block(dot_impl, ctx, tree, it, id, self_id);
			} break;

			// case SymbolKind::PROGRAM: {  // Top level node.
			// 	println(std::cout, "digraph G {");
			// 	println(std::cout, "  node [shape=Mrecord style=filled fillcolor=\"#bfbfbf\"];");
			// 		detail::edge(std::cout, parent_id, self_id, kind);
			// 		it = visit_block(dot_impl, ctx, tree, it, id, self_id);
			// 	println(std::cout, "}");
			// } break;

			default: return false;
		}

		return true;
	}

	inline std::vector<Symbol>::iterator dot(Context& ctx, std::vector<Symbol>& tree) {
		PV_LOG(LogLevel::OK);

		size_t id = 0;
		return visitor(dot_impl, ctx, tree, tree.begin(), id, 0ul);
	}
}

#endif

