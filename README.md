🦆 Pato — The Quacking Programming Language

Welcome to Pato, the most quacktastic programming language ever built!
Powered by pure C 🧠 and compiled straight down to x86-64 assembly 🪄.
Yes, you read that right — we don’t interpret, we generate real assembly and quack at the metal.


✨ Features That’ll Make You Quack

🗣️ quack for output:

quack expr; → print an integer 🧮

quack "hello"; → print a string literal 🦆


🔹 Variables:

let x = 42; → declare

x = x + 1; → assign


🔹 Arrays 🛠️:

let nums[10]; → fixed-size arrays

Works globally and on the stack 🏋️


🔹 Functions:

Up to 6 parameters (fast-register ABI, baby 🚀)

return statements, recursion-friendly


🔹 Control Flow:

if/else

while loops

break & continue


🔹 Expressions:

Math: + - * / %

Comparisons: == != < <= > >=

Logic: ! && || (with real short-circuiting)


🔹 Scopes {}: variables shadow outer ones like proper ducks in a pond

🔹 Assembly Output:

Generates Intel syntax x86-64

Uses printf for output

Fully ABI-compliant (SysV, Linux/macOS)


🏗️ Building the Compiler

# Clone the repo
git clone https://github.com/your-username/pato-lang.git
cd pato-lang

# Build the Pato compiler
gcc pato_compiler_plus.c -o pato

# Compile a Pato program into assembly
./pato < example.pato > out.s

# Assemble and run it
gcc out.s -o prog
./prog

> 🪟 Windows Users:
Use WSL2 or Mingw-w64. Ducks don’t swim well in vanilla CMD.exe.


🦆 Your First Pato Program (example.pato)

quack "Hello, Pato!";

let x = 5;
let y = 7;
quack x + y;  // 12

if (x < y) {
    quack "x < y";
} else {
    quack "nope";
}

let a[5];
let i = 0;
while (i < 5) {
    a[i] = i * 3;
    i = i + 1;
}
quack a[3]; // 9

Expected Output:

Hello, Pato!
12
x < y
9


🧪 Bigger Demo (demo.pato)

quack "Pato++ is live!";

let g = 10;
quack g;

let A[5];
A[0] = 7;
A[3] = 21;
quack A[3] % A[0];

fn sum(a, b) { return a + b; }
quack sum(20, 22);

fn test(n) {
  let v[4];
  let i = 0;
  while (i < 4) {
    if (i == 2) { i = i + 1; continue; }
    v[i] = (i + 1) * 100;
    if (i == n) { break; }
    i = i + 1;
  }
  return v[1];
}
quack test(99);

let x = 5;
{
  let x = 0;
  quack !x;    // 1
}
quack (x && (A[3] >= 21)); // 1

Expected Output:

Pato++ is live!
10
0
42
200
1
1


🧠 How It Works (A Quacktastic Overview)

Lexer: reads your .pato code, turns it into tasty tokens 🧩

Parser: builds an AST (Abstract Swim Tree 🪿)

Codegen: emits real x86-64 assembly

Assembler: GCC/Clang translates .s into executable duck magic

Runtime: Pato delegates printing to printf because ducks love C 🦆


🗺️ Roadmap

Feature	Status	ETA 🦆

Local arrays	✅ Done	Now!
Break & continue	✅ Done	Now!
String variables	🟡 Planned	Soon™
for loops	🟡 Planned	Soon™
Bitwise ops `&	^~`	🟡 Planned
Syscall backend (no libc)	🟡 Planned	Eventually 🐢


🧩 Example Use Cases

Learn compilers without touching dragons like LLVM 🐉

Experiment with real assembly output without hand-writing .s files

Impress friends:

> “I built my own language… it quacks.” 🦆


🧑‍💻 Contributing

We love pull requests and hate segfaults.

1. Fork this repo


2. Create a branch:

git checkout -b feat/duck-power


3. Hack, test, quack 🦆


4. Send a PR


Commit message style:

feat(pato): add quacktastic new feature
fix(parser): handle nested ducks properly
docs(readme): add more quacking examples


📜 License

MIT — Use it, modify it, share it.
Just remember to quack responsibly. 🦆


TL;DR

gcc pato_compiler_plus.c -o pato
./pato < example.pato > out.s
gcc out.s -o prog
./prog

> Ducks don’t fly without compiling. 🛠️🦆
