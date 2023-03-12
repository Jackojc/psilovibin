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
	Tree canonicalise_impl(
		Context& ctx,
		Tree tree,
		Tree::const_iterator current,
		Tree::const_iterator& it
	) {
		auto [sv, kind] = *current;

		switch (kind) {
			case SymbolKind::END: {
				// return { *current };
			} break;

			case SymbolKind::NONE:  // Literals.
			case SymbolKind::STRING:
			case SymbolKind::INTEGER: {
				return { *current };
			} break;

			case SymbolKind::IDENTIFIER: {
				if (auto sym_it = ctx.syms.find(sv); sym_it != ctx.syms.end()) {
					PV_LOG(LogLevel::OK, "found ref: ", sv);
					auto [view, subtree] = *sym_it;
					return subtree;
				}

				report(sv, ErrorKind::UNKNOWN_SYMBOL);
			} break;

			case SymbolKind::LEFT:  // Commands with arguments.
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME:
			case SymbolKind::UP:
			case SymbolKind::DOWN: {
				return visit(canonicalise_impl, ctx, tree, it);
			} break;

			case SymbolKind::SEQUENCE:  // Note patterns.
			case SymbolKind::PARALLEL: {
				return visit(canonicalise_impl, ctx, tree, it);
			} break;

			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				return visit(canonicalise_impl, ctx, tree, it);
			} break;

			case SymbolKind::MIDI: {  // Expressions/Statements with no arguments.
				return visit(canonicalise_impl, ctx, tree, it);
			} break;

			case SymbolKind::SELECT: {
				// TODO: We can remove either the MIDI or SELECT block here now since we have
				// canonicalised the tree and we don't expect to find an identifier inside a SELECT block.

				// auto [before, after] = visit_block_extent_inclusive(canonicalise_impl, ctx, tree, it);

				// PV_LOG(LogLevel::ERR, before->kind);
				// PV_LOG(LogLevel::ERR, after->kind);

				// it = ++after;
				return visit(canonicalise_impl, ctx, tree, it);
			} break;

			case SymbolKind::LET: {  // Expressions/Statements with arguments.
				Tree expr = visit(canonicalise_impl, ctx, tree, it);

				// Insert or re-assign symbol.
				if (auto [sym_it, succ] = ctx.syms.try_emplace(sv, expr); not succ) {
					auto& [view, sym] = *sym_it;
					sym = expr;
				}
			} break;

			default: {
				PV_LOG(LogLevel::WRN, "unhandled symbol: `", current->kind, "`");
			} break;
		}

		return {};
	}

	[[nodiscard]] inline Tree canonicalise(Context& ctx, Tree tree) {
		PV_LOG(LogLevel::OK);
		return pass(canonicalise_impl, ctx, tree);
	}
}

#endif

