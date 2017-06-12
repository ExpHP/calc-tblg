
// This is yet another implementation of the brute force HNF search,
// to accompany several other existing implementations floating around
// in various parts of the python and haskell regions of the code base.
// (I should do something about that)
// This one, however, is brutally optimized, CLI-based, and in rust.
//
// I misguidedly hacked this together on a friday, only to promptly
// throw it away after it occurred to me that the algorithm scales
// linearly with respect to the number of atoms, anyways, so there's
// no point optimizing.
//
// ...But then I found myself face to face with structures that have
// A LOT OF ATOMS; more than I could ever hope to handle running computations
// on. Yet these massive structures show up in contexts where I would like to
// know their volume.
// ...And that is the story of how this useless script somehow found a home.

// Input: a rational 2x2 matrix on stdin written like a nested json array.
//        Rationals can be written as '[a,b]' or 'a / b' or 'a'.
// Output: an integer lower-triangular HNF matrix on stdout that can be multiplied on the left
//         of the input matrix to produce an integer matrix.

#![feature(trace_macros)]
extern crate combine;
extern crate numtheory;
use ::std::io::prelude::*;

type Int = i32;
type Vec2<T> = (T,T);
type Vec3<T> = (T,T,T);
type Mat3<T> = Vec3<Vec3<T>>;

fn main() {
	let s = ::combine::primitives::from_read(::std::io::stdin());
	let s = ::combine::primitives::BufferedStream::new(s, 4096);
	let s = ::combine::State::new(s.as_stream());

	let (mat,_) = parse::rational_matrix_22_file(s).unwrap_or_else(|e| panic!("{:?}", e));
	let mat = mat.map(|v| v.map(|(a,b)| ratio::Ratio::new(a,b)));
	let hnf = hnf::search_hnf_prefactor_mat2(mat);
	println!("{}", hnf);
}

mod vec {
	use ::std::fmt;

	#[derive(Copy,Clone,Hash,PartialEq,Eq,PartialOrd,Ord,Debug)]
	pub struct Vec2<T>(pub T, pub T);
	#[derive(Copy,Clone,Hash,PartialEq,Eq,PartialOrd,Ord,Debug)]
	pub struct Vec3<T>(pub T, pub T, pub T);

	pub type Mat2<T> = Vec2<Vec2<T>>;
	pub type Mat3<T> = Vec3<Vec3<T>>;

	impl<T> Vec2<T> {
		pub fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> Vec2<U> {
			Vec2(f(self.0), f(self.1))
		}
	}
	impl<T> Vec3<T> {
		pub fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> Vec3<U> {
			Vec3(f(self.0), f(self.1), f(self.2))
		}
	}

	use ::std::ops::{Add,Mul,Neg};
	impl<T> Vec2<Vec2<T>> where T: Add<T,Output=T> + Mul<T,Output=T> + Neg<Output=T> + Copy {
		pub fn det(self) -> T {
			let Vec2(Vec2(m00,m01), Vec2(m10,m11)) = self;
			m00 * m11 + -(m10 * m01)
		}
	}
	impl<T> Vec3<Vec3<T>> where T: Add<T,Output=T> + Mul<T,Output=T> + Neg<Output=T> + Copy {
		pub fn det(self) -> T {
			let Vec3(Vec3(m00,m01,m02), Vec3(m10,m11,m12), Vec3(m20,m21,m22)) = self;
			(   m00 * m11 * m22
			+   m01 * m12 * m20
			+   m02 * m10 * m21
			+ -(m00 * m12 * m21)
			+ -(m01 * m10 * m22)
			+ -(m02 * m11 * m20)
			)
		}
	}

	impl<T: fmt::Display> fmt::Display for Vec2<T> {
		fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
			write!(f, "[{}, {}]", &self.0, &self.1)
		}
	}
	impl<T: fmt::Display> fmt::Display for Vec3<T> {
		fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
			write!(f, "[{}, {}, {}]", &self.0, &self.1, &self.2)
		}
	}
}

mod ratio {
	use ::numtheory::{gcd, extended_gcd, GcdData};
	use super::Int;

	#[derive(Copy,Clone,Hash,PartialEq,Eq,PartialOrd,Ord,Debug)]
	pub struct Ratio {
		numer: Int,
		denom: Int,
	}

	impl ::std::fmt::Display for Ratio {
		fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
			match self.denom {
				1 => write!(f, "{}", self.numer),
				_ => write!(f, "{}/{}", self.numer, self.denom),
			}
		}
	}

	fn coprime_parts(a: i32, b: i32) -> (i32,i32) {
		let GcdData { quotients: (a, b), .. } = extended_gcd(a, b);
		(a, b)
	}

	impl Ratio {
		pub fn new(numer: i32, denom: i32) -> Ratio {
			debug_assert!(denom != 0, "divide by zero");
			let (numer, denom) = (numer * denom.signum(), denom * denom.signum());
			let (numer, denom) = coprime_parts(numer, denom);
			Ratio::new_unchecked(numer, denom)
		}
		pub fn numer(self) -> i32 { self.numer }
		/// Denominator in simplest form
		pub fn denom(self) -> i32 { self.denom }
		pub fn zero() -> Ratio { Ratio { numer: 0, denom: 1 } }
		pub fn one()  -> Ratio { Ratio { numer: 1, denom: 1 } }

		fn new_unchecked(numer: i32, denom: i32) -> Ratio {
			// we'll still check in debug mode...
			debug_assert!(gcd(numer, denom) == 1, "numer and denom not coprime");
			debug_assert!(denom >= 1, "denom not positive");
			Ratio { numer, denom }
		}

		pub fn to_integer(self) -> Option<i32> {
			match self.denom {
				1 => Some(self.numer),
				_ => None,
			}
		}
	}

	impl ::std::ops::Add<Ratio> for Ratio {
		type Output = Ratio;
		fn add(self, other: Ratio) -> Ratio {
			Ratio::new(
				self.numer * other.denom + self.denom * other.numer,
				self.denom * other.denom,
			)
		}
	}

	/// Ratios can be multiplied directly by integers,
	/// at a potentially cheaper cost.
	/// TODO: Bench?
	impl ::std::ops::Mul<i32> for Ratio {
		type Output = Ratio;
		fn mul(self, b: i32) -> Ratio {
			let (new_denom, new_b) = coprime_parts(self.denom, b);
			Ratio::new_unchecked(self.numer * new_b, new_denom)
		}
	}

	impl ::std::ops::Mul<Ratio> for Ratio {
		type Output = Ratio;
		fn mul(self, other: Ratio) -> Ratio {
			Ratio::new(
				self.numer * other.numer,
				self.denom * other.denom,
			)
		}
	}

	impl ::std::ops::Neg for Ratio {
		type Output = Ratio;
		fn neg(self) -> Ratio {
			Ratio::new_unchecked(-self.numer, self.denom)
		}
	}

	/// Check if two ratios add to an integer.
	/// Theoretically cheaper than `(a + b).to_integer().is_some()`,
	///  which would needlessly compute a reduced form of the sum.
	/// FIXME: Empirical benchmark?
	pub fn sum_is_integer(a: Ratio, b: Ratio) -> bool {
		(a.denom == b.denom) && (a.numer + b.numer) % a.denom == 0
	}
}

mod hnf {
	use super::Int;
	use ::vec::{Mat2,Vec2};
	use ::ratio::Ratio;
	use numtheory::lcm;

	pub fn search_hnf_prefactor_mat2(mat: Mat2<Ratio>) -> Mat2<Int> {
		let Vec2(
			Vec2(m00,m01),
			Vec2(m10,m11),
		) = mat;

		let c00 = lcm(m00.denom(), m01.denom());

		let c10;
		let c11;
		let mut y = 0i32;
		'y: loop {
			y += 1;
			let sums = Vec2(m10, m11).map(|m| m * y);
			for x in 0..c00 {
				if ::ratio::sum_is_integer(sums.0, m00 * x) {
					if ::ratio::sum_is_integer(sums.1, m01 * x) {
						c10 = x;
						c11 = y;
						break 'y;
					}
				}
			}
		}
		Vec2(Vec2(c00,0),Vec2(c10,c11))
	}
}

mod parse {

	use ::combine::*;
	use ::combine::byte::*;
	use vec::{Vec2,Vec3,Mat2,Mat3};

	/// Equivalent to `a.and(b)`, but more symmetric.
	macro_rules! and {
		($a:expr, $b:expr, $c:expr, $d:expr, $e:expr $(,)*)
		=> { $a.and($b).and($c).and($d).and($e).map(|((((a,b),c),d),e)| (a,b,c,d,e)) };
		($a:expr, $b:expr, $c:expr, $d:expr $(,)*)
		=> { $a.and($b).and($c).and($d).map(|(((a,b),c),d)| (a,b,c,d)) };
		($a:expr, $b:expr, $c:expr $(,)*)
		=> { $a.and($b).and($c).map(|((a,b),c)| (a,b,c)) };
		($a:expr, $b:expr $(,)*)
		=> { $a.and($b) };
		($a:expr $(,)*)
		=> { $a };
	}

	/// Parse a vector of static size.
	/// Produces the item (length 1) or a tuple (length 2+)
	macro_rules! vec {
		(1, $beg:expr, $end:expr, $sep:expr, $ws:expr, $p:expr)
		=> {{
			let ws = || $ws;
			let p = || between(ws(), ws(), $p);
			between(
				and!(byte($beg)),
				and!(optional(byte($sep)), ws(), byte($end)),
				p(),
			)
		}};

		(2, $beg:expr, $end:expr, $sep:expr, $ws:expr, $p:expr)
		=> {{
			let ws = || $ws;
			let p = || between(ws(), ws(), $p);
			vec!(1, $beg, $end, $sep, $ws, and!(p(), byte($sep), p()))
				.map(|(a,_,b)| (a,b))
		}};

		(3, $beg:expr, $end:expr, $sep:expr, $ws:expr, $p:expr)
		=> {{
			let ws = || $ws;
			let p = || between(ws(), ws(), $p);
			vec!(1, $beg, $end, $sep, $ws, and!(p(), byte($sep), p(), byte($sep), p()))
				.map(|(a,_,b,_,c)| (a,b,c))
		}};

		// Vec2/Vec3-producing variants
		(Vec2, $beg:expr, $end:expr, $sep:expr, $ws:expr, $p:expr)
		=> { vec!(2, $beg, $end, $sep, $ws, $p).map(|(a,b)| Vec2(a,b)) };
		(Vec3, $beg:expr, $end:expr, $sep:expr, $ws:expr, $p:expr)
		=> { vec!(3, $beg, $end, $sep, $ws, $p).map(|(a,b,c)| Vec3(a,b,c)) };

		// add defaults
		($n:tt, $p:expr) => { vec!($n, b'[', b']', b',', skip_many(space()), $p) };
	}

	/// Parse a nonnegative integer.
	fn nonneg_integer<I>(s: I) -> ParseResult<i32, I>
	where I: Stream<Item=u8>
	{
		let char_digit = digit().map(|b| ::std::char::from_u32(b as u32).unwrap());
		many1(char_digit)
			.map(|s:String| s.parse::<i32>().unwrap())
			.parse_stream(s)
	}

	/// Parse a positive or negative integer.
	/// Spaces are not permitted after the minus sign.
	fn integer<I>(s: I) -> ParseResult<i32, I>
	where I: Stream<Item=u8>
	{
		and!(optional(byte(b'-')), parser(nonneg_integer))
		.map(|(minus,x)| match minus {
			Some(_) => -x,
			None => x,
		}).parse_stream(s)
	}

	/// Skip any amount of optional whitespace.
	fn ws<I>(s: I) -> ParseResult<(), I>
	where I: Stream<Item=u8>
	{ skip_many(space()).parse_stream(s) }

	/// Parse rational of the form "a / b", "[a, b]", or "a",
	/// permitting optional whitespace between all pairs of tokens.
	fn rational<I>(s: I) -> ParseResult<(i32,i32), I>
	where I: Stream<Item=u8>
	{
		let integer = || parser(integer);
		let ws = || parser(ws);

		// Parse rational of the form "a / b", with whitespace optional.
		let slash_rational =
			and!(integer(), ws(), byte(b'/'), ws(), integer())
			.map(|(a,_,_,_,b)| (a,b));

		// Parse rational of the form "[a , b]", with whitespace optional.
		let brace_rational = vec!(2, integer());

		choice!(
			try(slash_rational), // backtrack in case of plain integer
			integer().map(|x| (x,1)),
			brace_rational
		).parse_stream(s)
	}

	pub fn rational_matrix<I>(s: I) -> ParseResult<Mat3<(i32,i32)>, I>
	where I: Stream<Item=u8>
	{
		let rational = || parser(rational);
		let extend_mat22 = |((a,b),(c,d))| Vec3(
			Vec3(a, b, (0,1)),
			Vec3(c, d, (0,1)),
			Vec3((0,1), (0,1), (1,1)),
		);
		let mat33 = vec!(Vec3, vec!(Vec3, rational()));
		let mat22 = vec!(2, vec!(2, rational())).map(extend_mat22);
		let mut mat = try(mat22).or(mat33);

		mat.parse_stream(s)
	}

	pub fn rational_matrix_22<I>(s: I) -> ParseResult<Mat2<(i32,i32)>, I>
	where I: Stream<Item=u8>
	{
		vec!(Vec2, vec!(Vec2, parser(rational))).parse_stream(s)
	}

	// it would obviously be nicer to just expose a combinator like `ws_surround(p)`,
	// but I have no idea how to write a signature for that.
	/// Rational matrix, surrounded by optional whitespace.
	pub fn rational_matrix_22_file<I>(s: I) -> ParseResult<Mat2<(i32,i32)>, I>
	where I: Stream<Item=u8>
	{ and!(parser(ws), parser(rational_matrix_22), parser(ws)).map(|(_,m,_)| m).parse_stream(s) }
}

