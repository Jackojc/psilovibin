#ifndef PV_PASS_PRINTER_HPP
#define PV_PASS_PRINTER_HPP

/*
	HEADERS

	#include <iostream>
	#include <util.hpp>
	#include <pv.hpp>
*/

/*
	This pass pretty-prints the AST with a nested structure.
*/
namespace pv {
	// inline std::vector<Symbol>::iterator printer_impl(
	// 	Context& ctx,
	// 	std::vector<Symbol>::iterator it,
	// 	std::vector<Symbol>& tree,
	// 	size_t spaces = 0
	// ) {
	// 	const auto indent = [&] {
	// 		for (size_t i = 0; i != spaces; ++i)
	// 			print(std::cout, "  ");
	// 	};

	// 	if (it == tree.end())
	// 		return it;

	// 	std::vector<Symbol>::iterator current = it++;

	// 	switch (current->kind) {
	// 		case SymbolKind::NONE: {
	// 			indent(); println(std::cout, current->kind);
	// 		} break;

	// 		case SymbolKind::STRING:  // Literals.
	// 		case SymbolKind::INTEGER:
	// 		case SymbolKind::IDENTIFIER: {
	// 			indent(); println(std::cout, current->kind, " `", current->sv, "`");
	// 		} break;

	// 		case SymbolKind::GO:  // Commands with no arguments.
	// 		case SymbolKind::STOP:

	// 		case SymbolKind::LEFT:  // Commands with arguments.
	// 		case SymbolKind::RIGHT:
	// 		case SymbolKind::VELOCITY:
	// 		case SymbolKind::BPM:
	// 		case SymbolKind::TIME:

	// 		case SymbolKind::LET:  // Expressions/Statements.
	// 		case SymbolKind::INSTRUMENT:
	// 		case SymbolKind::CONTROL:
	// 		case SymbolKind::ACTION:
	// 		case SymbolKind::PROGRAM: {  // Top level node.
	// 			indent(); println(std::cout, current->kind, " `", current->sv, "`");
	// 				it = visit(ctx, it, tree, printer_impl, spaces + 1);
	// 			indent(); println(std::cout, SymbolKind::END);
	// 		} break;

	// 		default: {
	// 			PV_LOG(LogLevel::WRN, "unhandled symbol: `", current->kind, "`");
	// 		} break;
	// 	}

	// 	return it;
	// }

	bool printer_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator current,
		std::vector<Symbol>::iterator& it,
		size_t spaces = 0
	) {
		auto [sv, kind] = *current;

		const auto indent = [&] {
			for (size_t i = 0; i != spaces; ++i)
				print(std::cout, "  ");
		};

		switch (kind) {
			case SymbolKind::NONE: {
				indent(); println(std::cout, kind);
			} break;

			case SymbolKind::STRING:  // Literals.
			case SymbolKind::INTEGER:
			case SymbolKind::IDENTIFIER: {
				indent(); println(std::cout, kind, " `", sv, "`");
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
			case SymbolKind::ACTION:
			case SymbolKind::PROGRAM: {  // Top level node.
				indent(); println(std::cout, kind, " `", sv, "`");
					// it = visit(printer_impl, ctx, tree, it, spaces + 1);
					while (it->kind != SymbolKind::END)
						it = visitor(printer_impl, ctx, tree, it, spaces + 1);

					// return it + 1;
					it++;
				indent(); println(std::cout, SymbolKind::END);
			} break;

			default: {
				return false;
			} break;
		}

		return true;
	}

	inline std::vector<Symbol>::iterator printer(Context& ctx, std::vector<Symbol>& tree) {
		// return printer_impl(ctx, tree.begin(), tree);
		return visitor(printer_impl, ctx, tree, tree.begin());
	}
}

#endif

