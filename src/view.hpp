#ifndef PV_VIEW_HPP
#define PV_VIEW_HPP

/*
	HEADERS

	#include <iostream>
	#include <util.hpp>
*/

// View
namespace pv {
	struct View {
		const char* begin = nullptr;
		const char* end   = nullptr;

		constexpr View() {}

		constexpr View(const char* begin_, const char* end_):
			begin(begin_), end(end_) {}

		constexpr View(const char* begin_, size_t length):
			begin(begin_), end(begin_ + length) {}

		constexpr View(const char* ptr):
			begin(ptr), end(ptr + pv::length(ptr)) {}
	};

	constexpr size_t length(View sv) {
		return
			((sv.end - sv.begin) * (sv.end > sv.begin)) +
			((sv.begin - sv.end) * (sv.begin > sv.end));
	}

	constexpr bool cmp(View lhs, View rhs) {
		if (lhs.begin == rhs.begin and lhs.end == rhs.end)
			return true;

		if (length(lhs) != length(rhs))
			return false;

		for (size_t i = 0; i < length(lhs); i++) {
			if (*(lhs.begin + i) != *(rhs.begin + i))
				return false;
		}

		return true;
	}

	constexpr bool begins_with(View sv, View begins) {
		for (size_t i = 0; i < length(begins); i++) {
			if (*(sv.begin + i) != *(begins.begin + i))
				return false;
		}

		return true;
	}

	constexpr bool operator==(View lhs, View rhs) {
		return cmp(lhs, rhs);
	}

	constexpr bool operator!=(View lhs, View rhs) {
		return not(lhs == rhs);
	}

	[[nodiscard]] constexpr bool empty(View sv) {
		return sv.begin == sv.end;
	}

	inline std::ostream& operator<<(std::ostream& os, View sv) {
		os.write(sv.begin, length(sv));
		return os;
	}
}

constexpr pv::View operator""_sv(const char* str, size_t n) {
	return { str, str + n };
}

namespace std {
	template <> struct hash<pv::View> {
		constexpr size_t operator()(pv::View v) const {
			return pv::hash_bytes(v.begin, v.end);
		}
	};
}

namespace pv {
	[[nodiscard]] constexpr char chr(View sv) {
		return *sv.begin;
	}

	[[nodiscard]] constexpr View stretch(View lhs, View rhs) {
		return { lhs.begin, rhs.end };
	}

	[[nodiscard]] constexpr View next(View sv) {
		if (empty(sv)) return sv;
		return { sv.begin + 1, sv.end };
	}

	[[nodiscard]] constexpr View peek(View sv) {
		if (empty(sv)) return sv;
		return { sv.begin, sv.begin + 1 };
	}

	[[nodiscard]] constexpr View take(View& sv) {
		if (empty(sv)) return sv;
		const char* ptr = sv.begin;
		sv = next(sv);
		return { ptr, sv.begin };
	}

	template <typename F>
	[[nodiscard]] constexpr View take_while(View& sv, F&& fn) {
		View out { sv.begin, sv.begin };

		while (not empty(sv) and fn(peek(sv)))
			out = stretch(out, take(sv));

		return out;
	}
}

#endif

