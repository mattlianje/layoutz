#!/usr/bin/env ts-node

import { ul } from "./layoutz";

console.log("🔍 Testing UL nesting issues\n");

console.log("1. Simple UL:");
const simple = ul("Item 1", "Item 2", "Item 3");
console.log(simple.render());
console.log();

console.log("2. Single level nesting:");
const singleNested = ul("Parent", ul("Child"));
console.log(singleNested.render());
console.log();

console.log("3. Deep nesting (the problematic case):");
const deepNested = ul("Gegard", ul("Mousasi", ul("was a BAD man")));
console.log(deepNested.render());
console.log();

console.log("4. What it should look like manually:");
console.log("• Gegard");
console.log("  ◦ Mousasi");
console.log("    ▪ was a BAD man");
console.log();
