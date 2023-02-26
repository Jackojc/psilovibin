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

	bool fold_command_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator current,
		std::vector<Symbol>::iterator& it,
		std::vector<detail::OpStackEntry>& op_stack
	) {
		auto [sv, kind] = *current;

		switch (kind) {
			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				std::vector<Symbol>::iterator begin = it;
					it = visit_block(fold_command_impl, ctx, tree, it, op_stack);
				std::vector<Symbol>::iterator end = it;

				std::vector<detail::OpStackEntry>::reverse_iterator stack_it = std::find_if(op_stack.rbegin(), op_stack.rend(),
					[&] (detail::OpStackEntry entry) {
						return entry.kind == kind;
					});

				if (stack_it != op_stack.rend()) {
					auto [op_kind, op_begin, op_end] = *stack_it;

					if (std::equal(op_begin, op_end, begin, end)) {
						// Move current block to the end.
						std::vector<Symbol>::iterator new_end = std::rotate(current, end, tree.end());

						// Resize to remove last elements without invalidating iterators in op_entry.
						tree.resize(std::distance(tree.begin(), new_end));

						// We want to move the iterator back to the beginning of this block because we
						// moved the element underneath to the end and erased it.
						it = current;

						break;  // Don't add to op_stack since we've removed it.
					}
				}

				op_stack.emplace_back(kind, begin, end);
			} break;

			case SymbolKind::SELECT: {
				it = visit_block(fold_command_impl, ctx, tree, it, op_stack);
			} break;

			default: return false;
		}

		return true;
	}

	bool fold_select_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator current,
		std::vector<Symbol>::iterator& it,
		std::vector<detail::OpStackEntry>& op_stack
	) {
		auto [sv, kind] = *current;

		switch (kind) {
			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO:

			case SymbolKind::SELECT: {
				// op_stack.erase(std::remove_if(op_stack.begin(), op_stack.end(),
				// 	[] (detail::OpStackEntry entry) { return entry.kind != SymbolKind::SELECT; }), op_stack.end());

				std::vector<Symbol>::iterator begin = it;
					it = visit_block(fold_select_impl, ctx, tree, it, op_stack);
				std::vector<Symbol>::iterator end = it;

				std::vector<detail::OpStackEntry>::reverse_iterator stack_it = std::find_if(op_stack.rbegin(), op_stack.rend(),
					[&] (detail::OpStackEntry entry) {
						return entry.kind == kind;
					});

				if (stack_it != op_stack.rend()) {
					auto [op_kind, op_begin, op_end] = *stack_it;

					if (std::equal(op_begin, op_end, begin, end)) {
						// Move current block to the end.
						std::vector<Symbol>::iterator new_end = std::rotate(current, end, tree.rbegin().base());

						// Resize to remove last elements without invalidating iterators in op_entry.
						tree.resize(std::distance(tree.begin(), new_end));

						// We want to move the iterator back to the beginning of this block because we
						// moved the element underneath to the end and erased it.
						it = current;

						break;  // Don't add to op_stack since we've removed it.
					}
				}

				op_stack.emplace_back(kind, begin, end);
			} break;

			default: return false;
		}

		return true;
	}

	inline std::vector<Symbol>::iterator fold(Context& ctx, std::vector<Symbol>& tree) {
		PV_LOG(LogLevel::OK);

		std::vector<detail::OpStackEntry> op_stack;

		pass(fold_select_impl, ctx, tree, op_stack); op_stack.clear();
		// pass(fold_command_impl, ctx, tree, op_stack); op_stack.clear();

		return tree.end();  // TODO: Return iterator from pass here.
	}
}

#endif
