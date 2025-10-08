# Test {#main-title}

Second test with line breaks.  
This should break here.  
And here.

::: {.test-list}
- First unordered item 
- Second unordered item 
  - Nested subitem 
:::

::: {#ordered-items}
1. First ordered item 
2. Second ordered item
3. Third ordered item
:::

Inline math: $1 * 1 = 1$

Block math with attributes:

::: {#integral .math-block}
$$ 
\int_0^1 x^2 \, dx = \tfrac{1}{3}
$$
:::

## Subtitle Test {.subtitle}

Now a code block:

```haskell {.haskell}
main :: IO ()
main = putStrLn "Hello, Pandoc!"
```
