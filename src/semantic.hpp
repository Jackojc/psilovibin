#ifndef PV_PASS_SEMANTIC_HPP
#define PV_PASS_SEMANTIC_HPP

/*
	HEADERS

	#include <util.hpp>
	#include <pv.hpp>
*/

/*
	This pass checks for correct usage of types and resolves variable references aswell as
	canonicalising the AST.
*/
namespace pv {
	std::vector<Symbol>::iterator semantic_pattern_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator current,
		std::vector<Symbol>::iterator it
	) {
		auto [sv, kind] = *current;

		switch (kind) {
			case SymbolKind::INTEGER: break;

			case SymbolKind::SEQUENCE:
			case SymbolKind::PARALLEL: {
				it = visit_block(semantic_pattern_impl, ctx, tree, it);
			} break;

			default: {  // If we find anything else, it's an error.
				report(sv, ErrorKind::EXPECT_INTEGER);
			}
		}

		return it;
	}

	std::vector<Symbol>::iterator semantic_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator current,
		std::vector<Symbol>::iterator it
	) {
		auto [sv, kind] = *current;

		switch (kind) {
			case SymbolKind::NONE:
			case SymbolKind::STRING:  // Literals.
			case SymbolKind::INTEGER:
			case SymbolKind::IDENTIFIER: break;

			case SymbolKind::LEFT:  // Commands with arguments.
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME:
			case SymbolKind::UP:
			case SymbolKind::DOWN: {
				match(tree, it,
					SymbolKind::INTEGER, ErrorKind::EXPECT_INTEGER);

				it = visit_block(semantic_impl, ctx, tree, it);
			} break;

			case SymbolKind::SEQUENCE:
			case SymbolKind::PARALLEL: {  // Note patterns.
				// TODO: Ensure correct types
				// Possible solution here is to implement a function that iterates over all nodes
				// in a block. Perhaps more specifically all _leaf_ nodes in a block so we can ensure
				// that all of them are integers.
				it = visit_block(semantic_pattern_impl, ctx, tree, it);
			} break;

			// TODO: Make sure sequence has a BPM when we use GO. If we don't have a BPM at this point
			// how can we generate notes?
			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				it = visit_block(semantic_impl, ctx, tree, it);
			} break;

			case SymbolKind::MIDI: {  // Expressions/Statements with no arguments.
				// TODO: Check if MIDI channel is valid.
				// Maybe implement visitor for leaf nodes to check if channel here is valid
				// in the range specified by the MIDI standard.
				match(tree, it,
					SymbolKind::STRING, ErrorKind::EXPECT_STRING,
					SymbolKind::INTEGER, ErrorKind::EXPECT_INTEGER);

				it = visit_block(semantic_impl, ctx, tree, it);
			} break;

			case SymbolKind::SELECT: {
				match(tree, it,
					SymbolKind::MIDI, ErrorKind::EXPECT_MIDI,
					SymbolKind::STRING, ErrorKind::EXPECT_STRING,
					SymbolKind::INTEGER, ErrorKind::EXPECT_INTEGER);

				it = visit_block(semantic_impl, ctx, tree, it);
			} break;

			case SymbolKind::LET: {  // Expressions/Statements with arguments.
				it = visit_block(semantic_impl, ctx, tree, it);
			} break;

			default: {
				PV_LOG(LogLevel::WRN, "unhandled symbol: `", current->kind, "`");
			} break;
		}

		return it;
	}

	inline [[nodiscard]] std::vector<Symbol>::iterator semantic(Context& ctx, std::vector<Symbol>& tree) {
		PV_LOG(LogLevel::OK);
		return pass(semantic_impl, ctx, tree);
	}
}

#endif

