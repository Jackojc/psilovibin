#ifndef PV_PASS_FOLD_HPP
#define PV_PASS_FOLD_HPP

/*
	HEADERS

	#include <algorithm>
	#include <vector>
	#include <util.hpp>
	#include <pv.hpp>
*/

/*
	This pass tries to find adjacent blocks (of the same kind) that are identical so
	they can be merged to a single block.
*/
namespace pv {
	namespace detail {
		struct OpStackEntry {
			SymbolKind kind;
			std::vector<Symbol>::iterator begin;
			std::vector<Symbol>::iterator end;

			constexpr OpStackEntry(
				SymbolKind kind_,
				std::vector<Symbol>::iterator begin_,
				std::vector<Symbol>::iterator end_
			):
				kind(kind_),
				begin(begin_),
				end(end_) {}
		};
	}

	bool fold_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator current,
		std::vector<Symbol>::iterator& it,
		std::vector<detail::OpStackEntry>& op_stack
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
			case SymbolKind::TIME: {
				it = visit_block(fold_impl, ctx, tree, it, op_stack);
			} break;

			case SymbolKind::PATTERN: {  // Note patterns.
				it = visit_block(semantic_pattern_impl, ctx, tree, it);
			} break;

			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				it = visit_block(fold_impl, ctx, tree, it, op_stack);
			} break;

			case SymbolKind::MIDI: {  // Expressions/Statements with no arguments.
				it = visit_block(fold_impl, ctx, tree, it, op_stack);
			} break;

			case SymbolKind::SELECT: {
				std::vector<Symbol>::iterator begin = it;
					it = visit_block(fold_impl, ctx, tree, it, op_stack);
				std::vector<Symbol>::iterator end = it;

				std::vector<detail::OpStackEntry>::reverse_iterator stack_it = std::find_if(op_stack.rbegin(), op_stack.rend(),
					[&] (detail::OpStackEntry entry) {
						return entry.kind == kind;
					});

				if (stack_it != op_stack.rend()) {
					auto [op_kind, op_begin, op_end] = *stack_it;

					if (std::equal(op_begin, op_end, begin, end))
						PV_LOG(LogLevel::ERR, "candidate");
				}

				op_stack.emplace_back(kind, begin, end);
			} break;

			case SymbolKind::LET: {  // Expressions/Statements with arguments.
				it = visit_block(fold_impl, ctx, tree, it, op_stack);
			} break;

			default: return false;
		}

		return true;
	}

	inline std::vector<Symbol>::iterator fold(Context& ctx, std::vector<Symbol>& tree) {
		PV_LOG(LogLevel::OK);

		std::vector<detail::OpStackEntry> op_stack;
		return pass(fold_impl, ctx, tree, op_stack);
	}
}

#endif
