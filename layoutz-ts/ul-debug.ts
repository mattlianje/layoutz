#!/usr/bin/env ts-node

import { ul, UnorderedList, Text } from "./layoutz";

console.log("🔍 Debugging UL construction\n");

// Let's build the nested structure step by step
console.log("1. Creating innermost list:");
const innermost = ul("was a BAD man");
console.log("Type:", innermost.constructor.name);
console.log(
  "Items:",
  (innermost as any).items.map(
    (item: any) => `${item.constructor.name}: ${item.content || item.render()}`
  )
);
console.log("Render:", innermost.render());
console.log();

console.log("2. Creating middle list:");
const middle = ul("Mousasi", innermost);
console.log("Type:", middle.constructor.name);
console.log(
  "Items:",
  (middle as any).items.map(
    (item: any) =>
      `${item.constructor.name}: ${item.content || "UnorderedList"}`
  )
);
console.log("Render:", middle.render());
console.log();

console.log("3. Creating outer list:");
const outer = ul("Gegard", middle);
console.log("Type:", outer.constructor.name);
console.log(
  "Items:",
  (outer as any).items.map(
    (item: any) =>
      `${item.constructor.name}: ${item.content || "UnorderedList"}`
  )
);
console.log("Render:", outer.render());
console.log();

console.log("4. What we expect:");
console.log("• Gegard");
console.log("  ◦ Mousasi");
console.log("    ▪ was a BAD man");
console.log();
