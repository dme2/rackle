// A Simple C++ co-yield Example
/*

Coroutines, to put it succinctly, are functions that can be paused and resumed at user defined suspension points. The coroutines in C++ are stack free, this allows for decent performance. I won't go into too much detail regarding the implementation here, you can check out [cpp_reference](https://en.cppreference.com/w/cpp/language/coroutines) or [lewis_baker's blog](https://lewissbaker.github.io/) for more details on that.

So How do coroutines help us? In general, they provide a nice interface for functions that might require starting and stopping, with the function's state being maintained in between calls. Think of how a rendering callback might work in a game engine. Every iteration or tick, the rendering callback function is called which might fill in the necessary data (like object positions, lighting calculations, etc.) for drawing to a screen.

In general the usual use cases for coroutines are asynchronous IO (networks, files, etc.) and generator functions (audio, images, etc.), which we'll be taking a look at soon.

The following example isn't perfect. It doesn't speed up execution nor does it improve readability very much, but it does show how coroutines (specifically lazy generators) can be used. The following Generator struct is part of the compiler magic necessary to use coroutines in c++. Note - the struct's name doesn't necessarily have to be "Generator", in can be anything, "Generator" just aptly describes the type of function we'll be creating.
 */
#include <coroutine>
#include <cstdio>
#include <cstdint>
#include <cstring>
#include <exception>
#include <iostream>
#include <unistd.h>
#include <utility>

template <typename T>
struct Generator {
  struct promise_type;
  // the handle is for suspending and resuming the coroutine
  using handle_type = std::coroutine_handle<promise_type>;

  // the promise type handles the returnable information for our coro
  struct promise_type {
	T val_; // value to be yielded or returned from our coro
	std::exception_ptr exception_;

	// necessary function for the promise type
	Generator get_return_object() {
	  return Generator(handle_type::from_promise(*this));
	}

	// these std::suspend_always functions control the behavior of the coro on suspensions
	std::suspend_always initial_suspend() {return {}; }
	std::suspend_always final_suspend() noexcept {return {};}

	// error handling
	void unhandled_exception() { exception_ = std::current_exception(); }

	// yields the value
	template<std::convertible_to<T> From>
	std::suspend_always yield_value (From&& from) {
	  val_ = std::forward<From>(from);
	  return {};
	}

	void return_void() {}
  };

  handle_type h_;

  Generator(handle_type h) : h_(h) {};

  ~Generator() { h_.destroy(); }

  // this what happens when we call our coro function (e.g. f())
  T operator()() {
	h_(); // call our promise type. this will run yield_value (I think?)
	return std::move(h_.promise().val_); // return the data
  }
};
/*
The following code calculates the donuts current frame. It's lifted directly from [here] (https://www.a1k0n.net/2021/01/13/optimizing-donut.html), with a nice explanation found [here](https://www.a1k0n.net/2011/07/20/donut-math.html) */
#define R(mul,shift,x,y) \
  _=x; \
  x -= mul*y>>shift; \
  y += mul*_>>shift; \
  _ = 3145728-x*x-y*y>>11; \
  x = x*_>>10; \
  y = y*_>>10;


Generator<int8_t*>
get_donut_frame() {
  int8_t b[1760], z[1760];
  int sA=1024,cA=0,sB=1024,cB=0,_;
  for (;;) {
    memset(b, 32, 1760);  // text buffer
    memset(z, 127, 1760);   // z buffer
    int sj=0, cj=1024;
    for (int j = 0; j < 90; j++) {
      int si = 0, ci = 1024;  // sine and cosine of angle i
      for (int i = 0; i < 324; i++) {
        int R1 = 1, R2 = 2048, K2 = 5120*1024;

        int x0 = R1*cj + R2,
            x1 = ci*x0 >> 10,
            x2 = cA*sj >> 10,
            x3 = si*x0 >> 10,
            x4 = R1*x2 - (sA*x3 >> 10),
            x5 = sA*sj >> 10,
            x6 = K2 + R1*1024*x5 + cA*x3,
            x7 = cj*si >> 10,
            x = 40 + 30*(cB*x1 - sB*x4)/x6,
            y = 12 + 15*(cB*x4 + sB*x1)/x6,
            N = (-cA*x7 - cB*((-sA*x7>>10) + x2) - ci*(cj*sB >> 10) >> 10) - x5 >> 7;

        int o = x + 80 * y;
        int8_t zz = (x6-K2)>>15;
        if (22 > y && y > 0 && x > 0 && 80 > x && zz < z[o]) {
          z[o] = zz;
          b[o] = ".,-~:;=!*#$@"[N > 0 ? N : 0];
        }
        R(5, 8, ci, si)  // rotate i
      }
      R(9, 7, cj, sj)  // rotate j
    }
	co_yield(b); // suspension point
    R(5, 7, cA, sA);
    R(5, 8, cB, sB);
    usleep(15000);
    printf("\x1b[23A");
  }
}

/*
  The co_yield(b) call is the function's suspension point. co_yield suspends the function and returns (yields) the value we pass to it. In this case, we're passing a char array filled with the donut's current frame data. By invoking the generator (calling gen()), we cause the function to execute until the suspension point. Reinvoking the generator will cause the function to continue where it left off. In this case, calling gen() will result in the function executing the rest of the code starting at the end of the loop, iterating through the loop again and pausing at the suspension point.
 */

int main() {
  auto gen = get_donut_frame();
  for(;;) {
	auto b_ = gen();
	for (int k = 0; 1761 > k; k++)
      putchar(k % 80 ? b_[k] : 10);
  }

  return 0;
}

/*
  In our main function we invoke the generator inside of an infinite for loop. Calling gen() yields our desired buffer which we then print out to the console. Resulting in a rotating donut being drawn on the screen.
 */
