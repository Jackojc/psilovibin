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
	bool semantic_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		// View sv,
		// SymbolKind kind,
		std::vector<Symbol>::iterator current,
		std::vector<Symbol>::iterator& it
	) {
		auto [sv, kind] = *current;

		switch (kind) {
			case SymbolKind::NONE: {

			} break;

			case SymbolKind::STRING:  // Literals.
			case SymbolKind::INTEGER: {

			} break;

			case SymbolKind::IDENTIFIER: {
				// TODO: Just replace identifier with stored sub-tree. We can worry about
				// whether or not it's correct later on. This should simplify things a lot
				// because we wont require duplicated code in all block-like nodes to replace
				// identifiers with their corresponding sub-tree.

				if (auto sym_it = ctx.syms.find(sv); sym_it != ctx.syms.end()) {
					auto [view, subtree] = *sym_it;

					it = tree.erase(current);
					it = tree.insert(it, subtree.begin(), subtree.end());
				}

				else {
					report(sv, ErrorKind::UNKNOWN_SYMBOL);
				}
			} break;

			case SymbolKind::LEFT:  // Commands with arguments.
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME: {
				// TODO: Ensure correct types
				// TODO: Resolve any symbols to values to canonicalise tree.
				it = visit_block(semantic_impl, ctx, tree, it);
			} break;

			case SymbolKind::PATTERN: {  // Note patterns.
				// TODO: Ensure correct types
				it = visit_block(semantic_impl, ctx, tree, it);
			} break;

			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				it = visit_block(semantic_impl, ctx, tree, it);
			} break;

			case SymbolKind::MIDI: {  // Expressions/Statements with no arguments.
				// TODO: Check if MIDI channel is valid.
				it = visit_block(semantic_impl, ctx, tree, it);
			} break;

			case SymbolKind::SELECT: {
				// TODO: Verify that this block is a MIDI device and not an identifier or something else.
				// Maybe we can implement a function to compare the node types of two trees to do easy
				// matching.
				it = visit_block(semantic_impl, ctx, tree, it);
			} break;

			case SymbolKind::LET: {  // Expressions/Statements with arguments.
				std::vector<Symbol>::iterator begin = it;
					it = visit_block(semantic_impl, ctx, tree, it);
				std::vector<Symbol>::iterator end = it - 1;

				// Insert or re-assign symbol.
				if (auto [sym_it, succ] = ctx.syms.try_emplace(sv, begin, end); not succ) {
					PV_LOG(LogLevel::WRN, "reassign variable: ", sv);
					auto& [view, sym] = *sym_it;
					sym.assign(begin, end);
				}
			} break;

			default: return false;
		}

		return true;
	}

	inline std::vector<Symbol>::iterator semantic(Context& ctx, std::vector<Symbol>& tree) {
		PV_LOG(LogLevel::OK);
		return pass(semantic_impl, ctx, tree);
	}
}

#endif

