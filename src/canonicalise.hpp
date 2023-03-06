#ifndef PV_PASS_CANONICALISE_HPP
#define PV_PASS_CANONICALISE_HPP

/*
	HEADERS

	#include <util.hpp>
	#include <pv.hpp>
*/

/*
	This pass removes any references to variables or other symbols and leaves the
	tree in a canonicalised form where we can make a lot more assumptions about
	its structure.
*/
namespace pv {
	std::vector<Symbol>::iterator canonicalise_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator current,
		std::vector<Symbol>::iterator it
	) {
		auto [sv, kind] = *current;

		switch (kind) {
			case SymbolKind::NONE: {

			} break;

			case SymbolKind::STRING:  // Literals.
			case SymbolKind::INTEGER: {

			} break;

			case SymbolKind::IDENTIFIER: {
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
			case SymbolKind::TIME:
			case SymbolKind::UP:
			case SymbolKind::DOWN: {
				it = visit_block(canonicalise_impl, ctx, tree, it);
			} break;

			case SymbolKind::SEQUENCE:  // Note patterns.
			case SymbolKind::PARALLEL: {
				it = visit_block(canonicalise_impl, ctx, tree, it);
			} break;

			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				it = visit_block(canonicalise_impl, ctx, tree, it);
			} break;

			case SymbolKind::MIDI: {  // Expressions/Statements with no arguments.
				it = visit_block(canonicalise_impl, ctx, tree, it);
			} break;

			case SymbolKind::SELECT: {
				// TODO: We can remove either the MIDI or SELECT block here now since we have
				// canonicalised the tree and we don't expect to find an identifier inside a SELECT block.

				auto [before, after] = visit_block_extent_inclusive(canonicalise_impl, ctx, tree, it);

				PV_LOG(LogLevel::ERR, before->kind);
				PV_LOG(LogLevel::ERR, after->kind);

				it = ++after;
			} break;

			case SymbolKind::LET: {  // Expressions/Statements with arguments.
				auto [before, after] = visit_block_extent_inclusive(canonicalise_impl, ctx, tree, it);

				// Insert or re-assign symbol.
				if (auto [sym_it, succ] = ctx.syms.try_emplace(sv, before, after); not succ) {
					auto& [view, sym] = *sym_it;
					sym.assign(before, after);
				}

				it = ++after;
				it = tree.erase(current, after);  // Erase this assignment from the tree since we have stored it.
			} break;

			default: {
				PV_LOG(LogLevel::WRN, "unhandled symbol: `", current->kind, "`");
			} break;
		}

		return it;
	}

	inline std::vector<Symbol>::iterator canonicalise(Context& ctx, std::vector<Symbol>& tree) {
		PV_LOG(LogLevel::OK);
		return pass(canonicalise_impl, ctx, tree);
	}
}

#endif

