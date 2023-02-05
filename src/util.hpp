#ifndef PV_UTIL_HPP
#define PV_UTIL_HPP

/*
	HEADERS

	#include <iostream>
	#include <array>
	#include <string_view>
	#include <string>
*/

// Macros
namespace pv {
	#define PV_STR_IMPL_(x) #x
	#define PV_STR(x) PV_STR_IMPL_(x)

	#define PV_CAT_IMPL_(x, y) x##y
	#define PV_CAT(x, y) PV_CAT_IMPL_(x, y)

	#define PV_VAR(x) PV_CAT(var_, PV_CAT(x, PV_CAT(__LINE__, _)))

	#define PV_TRACE "[" __FILE__ ":" PV_STR(__LINE__) "] "
}

// I/O
namespace pv {
	namespace detail {
		template <typename... Ts>
		inline std::ostream& print_impl(std::ostream& os, Ts&&... xs) {
			return ((os << std::forward<Ts>(xs)), ..., os);
		}

		template <typename... Ts>
		inline std::ostream& println_impl(std::ostream& os, Ts&&... xs) {
			return ((os << std::forward<Ts>(xs)), ..., (os << '\n'));
		}
	}

	template <typename T, typename... Ts>
	inline std::ostream& print(std::ostream& os, T&& x, Ts&&... xs) {
		return detail::print_impl(os, std::forward<T>(x), std::forward<Ts>(xs)...);
	}

	template <typename T, typename... Ts>
	inline std::ostream& println(std::ostream& os, T&& x, Ts&&... xs) {
		return detail::println_impl(os, std::forward<T>(x), std::forward<Ts>(xs)...);
	}
}

// Logging
namespace pv {
	#ifndef PV_ANSI_DISABLE
		#define PV_RESET   "\x1b[0m"
		#define PV_BOLD    "\x1b[1m"

		#define PV_BLACK   "\x1b[30m"
		#define PV_RED     "\x1b[31m"
		#define PV_GREEN   "\x1b[32m"
		#define PV_YELLOW  "\x1b[33m"
		#define PV_BLUE    "\x1b[34m"
		#define PV_MAGENTA "\x1b[35m"
		#define PV_CYAN    "\x1b[36m"
		#define PV_WHITE   "\x1b[37m"

		#define PV_ERR     "\x1b[31;1m"
		#define PV_OK      "\x1b[34m"
	#else
		#define PV_RESET   ""
		#define PV_BOLD    ""

		#define PV_BLACK   ""
		#define PV_RED     ""
		#define PV_GREEN   ""
		#define PV_YELLOW  ""
		#define PV_BLUE    ""
		#define PV_MAGENTA ""
		#define PV_CYAN    ""
		#define PV_WHITE   ""

		#define PV_ERR     ""
		#define PV_OK      ""
	#endif

	#ifndef NDEBUG
		namespace detail {
			template <typename T> inline decltype(auto) dbg_impl(
				const char* file,
				const char* line,
				const char* expr_s,
				T&& expr
			) {
				println(std::cerr,
					"[", file, ":", line, "] ", expr_s, " = ", std::forward<T>(expr)
				);

				return std::forward<T>(expr);
			}
		}

		#define PV_DBG(expr) \
			(pv::detail::dbg_impl( \
				__FILE__, PV_STR(__LINE__), PV_STR(expr), (expr) \
			))

		#define PV_DBG_RUN(expr) \
			do { ((expr)); } while (0)
	#else
		#define PV_DBG(expr) ((expr))
		#define PV_DBG_RUN(expr) do {} while (0)
	#endif

	#define LOG_LEVELS \
		X(INF, PV_RESET "[-]") \
		X(WRN, PV_BLUE  "[*]") \
		X(ERR, PV_RED   "[!]") \
		X(OK,  PV_GREEN "[^]")

	#define X(a, b) a,
		enum class LogLevel: size_t { LOG_LEVELS };
	#undef X

	namespace detail {
		#define X(a, b) std::string_view { b },
			constexpr std::array LOG_TO_STR = { LOG_LEVELS };
		#undef X

		constexpr std::string_view log_to_str(LogLevel x) {
			return LOG_TO_STR[static_cast<size_t>(x)];
		}

		constexpr size_t log_padding() {
			return std::max_element(detail::LOG_TO_STR.begin(), detail::LOG_TO_STR.end())->size();
		}
	}

	inline std::ostream& operator<<(std::ostream& os, LogLevel x) {
		return print(os, detail::log_to_str(x));
	}

	#define PV_LOG(...) \
		do { [PV_VAR(fn_name) = __func__] (LogLevel PV_VAR(x), auto&&... PV_VAR(args)) { \
			PV_DBG_RUN(( \
				pv::print(std::cerr, PV_VAR(x), " ", PV_TRACE) \
			)); \
			PV_DBG_RUN(( pv::print(std::cerr, "`", PV_VAR(fn_name), "`" PV_RESET) )); \
			if constexpr(sizeof...(PV_VAR(args)) > 0) { PV_DBG_RUN( \
				(pv::print(std::cerr, " ", std::forward<decltype(PV_VAR(args))>(PV_VAR(args))...)) \
			); } \
			PV_DBG_RUN(( pv::print(std::cerr, '\n') )); \
		} ( __VA_ARGS__ ); } while (0)
}

// Utilities
namespace pv {
	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) or std::forward<Ts>(rest)) or ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) and std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool none(T&& first, Ts&&... rest) {
		return ((not std::forward<T>(first) and not std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool cmp_all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool cmp_any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) or ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool cmp_none(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) != std::forward<Ts>(rest)) and ...);
	}

	constexpr size_t length(const char* str) {
		const char *ptr = str;

		while (*ptr)
			++ptr;

		return ptr - str;
	}

	// FNV-1a
	constexpr auto hash_bytes(const char* begin, const char* const end) {
		size_t offset_basis = 14'695'981'039'346'656'037u;
		size_t prime = 1'099'511'628'211u;

		size_t hash = offset_basis;

		while (begin != end) {
			hash = (hash ^ static_cast<size_t>(*begin)) * prime;
			begin++;
		}

		return hash;
	}
}

#endif

