const data = [
  [
    {
      type: "Iterator<Item = T>",
      path: "std/iter/trait.Iterator.html",
      groups: [
        {
          comment: "Mapping and filtering",
          items: [
            "map        (( T) -> U)         -> Iterator<Item = U>",
            "filter     ((&T) -> bool)      -> Iterator<Item = T>",
            "filter_map (( T) -> Option<U>) -> Iterator<Item = U>",
          ],
        },
        {
          comment: "Collecting and folding",
          items: [
            "fold (S, (S, T) -> S) -> S",
            "collect () -> B where B: FromIterator<T>",
            "partition ((&T) -> bool) -> (B, B) where B: Default + Extend<T>",
          ],
        },
        {
          comment: "Counting and enumerating",
          items: [
            "count () -> usize",
            "last () -> Option<T>",
            "enumerate () -> Iterator<Item = (usize, T)>",
          ],
        },
        {
          comment: "Combining with other iterators",
          items: [
            "zip   (IntoIterator<Item = U>) -> Iterator<Item = (T, U)>",
            "chain (IntoIterator<Item = T>) -> Iterator<Item = T>",
          ],
        },
        {
          comment: "Flattening",
          items: [
            "flatten () -> Iterator<U> where T: IntoIterator<U>",
            "flat_map ((T) -> IntoIterator<Item = U>) -> Iterator<Item = U>",
          ],
        },
        {
          comment: "Taking and skipping",
          items: [
            "skip (usize) -> Iterator<Item = T>",
            "take (usize) -> Iterator<Item = T>",
            "skip_while ((&T) -> bool) -> Iterator<Item = T>",
            "take_while ((&T) -> bool) -> Iterator<Item = T>",
            "step_by (usize) -> Iterator<Item = T>",
          ],
        },
        {
          comment: "Misc. iterating",
          items: [
            "for_each ((T) -> ()) -> ()",
            "inspect ((&T) -> ()) -> Iterator<Item = T>",
            "scan (S, (&mut S, T) -> Option<U>) -> Iterator<Item = U>",
          ],
        },
        {
          comment: "Calculations",
          items: [
            "sum     () -> S where S: Sum<T>",
            "product () -> P where P: Product<T>",
          ],
        },
        {
          comment: "Maximum and minimum",
          items: [
            "max () -> Option<T> where T: Ord",
            "min () -> Option<T> where T: Ord",
            "max_by ((&T, &T) -> Ordering) -> Option<T>",
            "min_by ((&T, &T) -> Ordering) -> Option<T>",
            "max_by_key ((&T) -> U) -> Option<T> where U: Ord",
            "min_by_key ((&T) -> U) -> Option<T> where U: Ord",
          ],
        },
        {
          comment: "Comparing with another iterator",
          items: [
            "eq (IntoIterator<Item = T>) -> bool where T: PartialEq",
            "ne (IntoIterator<Item = T>) -> bool where T: PartialEq",
            "lt (IntoIterator<Item = T>) -> bool where T: PartialOrd",
            "le (IntoIterator<Item = T>) -> bool where T: PartialOrd",
            "gt (IntoIterator<Item = T>) -> bool where T: PartialOrd",
            "ge (IntoIterator<Item = T>) -> bool where T: PartialOrd",
            "cmp         (IntoIterator<Item = T>) ->        Ordering  where T: Ord",
            "partial_cmp (IntoIterator<Item = T>) -> Option<Ordering> where T: PartialOrd",
          ],
        },
        {
          comment: "Reverting and cycling",
          items: [
            "rev   () -> Iterator<Item = T> where Self: DoubleEndedIterator",
            "cycle () -> Iterator<Item = T> where Self: Clone",
          ],
        },
      ],
    },
    {
      type: "Iterator<Item = &T>",
      path: "std/iter/trait.Iterator.html",
      groups: [
        {
          comment: "Cloning inner",
          items: [
            "cloned () -> Iterator<T> where T: Clone",
          ],
        }
      ],
    },
    {
      type: "&mut Iterator<Item = T>",
      path: "std/iter/trait.Iterator.html",
      groups: [
        {
          comment: "Finding and positioning",
          items: [
            "find      ((&T) -> bool)      -> Option<T>",
            "find_map  (( T) -> Option<U>) -> Option<U>",
            "position  (( T) -> bool)      -> Option<usize>",
            "rposition (( T) -> bool)      -> Option<usize>\n" +
              "where Self: ExactSizeIterator + DoubleEndedIterator",
          ],
        },
        {
          comment: "Boolean operations",
          items: [
            "all ((T) -> bool) -> bool",
            "any ((T) -> bool) -> bool",
          ],
        },
      ],
    },
    {
      mod: "std::iter",
      path: "std/iter/",
      groups: [
        {
          comment: "Creating simple iterators",
          items: [
            "empty () -> Iterator<Item = T>",
            "once (T) -> Iterator<Item = T>",
            "repeat (T) -> Iterator<Item = T> where T: Clone",
            "repeat_with (() -> T) -> Iterator<Item = T>",
            "from_fn (() -> Option<T>) -> Iterator<Item = T>",
            "successors (Option<T>, (&T) -> Option<T>) -> Iterator<Item = T>",
          ],
        },
      ],
    },
  ],
  [
    {
      type: "&[T]",
      path: "std/primitive.slice.html",
      groups: [
        {
          comment: "Splitting to iterator",
          items: [
            "split  ((&T) -> bool) -> Iterator<Item = &[T]>",
            "rsplit ((&T) -> bool) -> Iterator<Item = &[T]>",
            "splitn  (usize, (&T) -> bool) -> Iterator<Item = &[T]>",
            "rsplitn (usize, (&T) -> bool) -> Iterator<Item = &[T]>",
          ],
        },
        {
          comment: "Splitting at position",
          items: [
            "split_at (usize) -> (&[T], &[T])",
            "split_first () -> Option<(&T, &[T])>",
            "split_last  () -> Option<(&T, &[T])>",
          ],
        },
        {
          comment: "Chunks and windows",
          items: [
            "chunks        (usize) -> Iterator<Item = &[T]>",
            "chunks_exact  (usize) -> Iterator<Item = &[T]>",
            "rchunks       (usize) -> Iterator<Item = &[T]>",
            "rchunks_exact (usize) -> Iterator<Item = &[T]>",
            "windows       (usize) -> Iterator<Item = &[T]>",
          ],
        },
        {
          comment: "Binary searching",
          items: [
            "binary_search (&T)                   -> Result<usize, usize> where T: Ord",
            "binary_search_by ((&T) -> Ordering)  -> Result<usize, usize>",
            "binary_search_by_key (&B, (&T) -> B) -> Result<usize, usize> where B: Ord",
          ],
        },
        {
          comment: "Getting and iterating",
          items: [
            "first () -> Option<&T>",
            "last  () -> Option<&T>",
            "get (usize) -> Option<&T>",
            "get (RangeBounds<usize>) -> Option<&[T]>",
            "iter () -> Iterator<Item = &T>",
          ],
        },
      ],
    },
    {
      type: "&mut [T]",
      path: "std/primitive.slice.html",
      groups: [
        {
          comment: "Splitting to iterator",
          items: [
            "split_mut  ((&T) -> bool) -> Iterator<Item = &mut [T]>",
            "rsplit_mut ((&T) -> bool) -> Iterator<Item = &mut [T]>",
            "splitn_mut  (usize, (&T) -> bool) -> Iterator<Item = &mut [T]>",
            "rsplitn_mut (usize, (&T) -> bool) -> Iterator<Item = &mut [T]>",
          ],
        },
        {
          comment: "Splitting at position",
          items: [
            "split_at_mut (usize) -> (&mut [T], &mut [T])",
            "split_first_mut () -> Option<(&mut T, &mut [T])>",
            "split_last_mut  () -> Option<(&mut T, &mut [T])>",
          ],
        },
        {
          comment: "Chunks",
          items: [
            "chunks_mut        (usize) -> Iterator<Item = &mut [T]>",
            "chunks_exact_mut  (usize) -> Iterator<Item = &mut [T]>",
            "rchunks_mut       (usize) -> Iterator<Item = &mut [T]>",
            "rchunks_exact_mut (usize) -> Iterator<Item = &mut [T]>",
          ],
        },
        {
          comment: "Sorting",
          items: [
            "sort () where T: Ord",
            "sort_by ((&T, &T) -> Ordering)",
            "sort_by_key ((&T) -> K) where K: Ord",
            "sort_by_cached_key ((&T) -> K) where K: Ord",
            "sort_unstable () where T: Ord",
            "sort_unstable_by ((&T, &T) -> Ordering)",
            "sort_unstable_by_key ((&T) -> K) where K: Ord",
          ],
        },
        {
          comment: "Rearranging",
          items: [
            "swap (usize, usize)",
            "reverse ()",
            "rotate_left (usize)",
            "rotate_right (usize)",
          ],
        },
        {
          comment: "Getting and iterating",
          items: [
            "first_mut () -> Option<&mut T>",
            "last_mut  () -> Option<&mut T>",
            "get_mut (usize) -> Option<&mut T>",
            "get_mut (RangeBounds<usize>) -> Option<&mut [T]>",
            "iter_mut () -> Iterator<Item = &mut T>",
          ],
        },
      ],
    },
  ],
  [
    {
      type: "Option<T>",
      path: "std/option/enum.Option.html",
      groups: [
        {
          comment: "To inner type",
          items: [
            "unwrap () -> T",
            "unwrap_or (T) -> T",
            "unwrap_or_else (() -> T) -> T",
            "unwrap_or_default () -> T where T: Default",
            "expect (&str) -> T",
          ],
        },
        {
          comment: "Converting to another type",
          items: [
            "map ((T) -> U) -> Option<U>",
            "map_or (U, (T) -> U) -> U",
            "map_or_else (() -> U, (T) -> U) -> U",
          ],
        },
        {
          comment: "To Result",
          items: [
            "ok_or (E) -> Result<T, E>",
            "ok_or_else (() -> E) -> Result<T, E>",
          ],
        },
        {
          comment: "Conditioning",
          items: [
            "filter ((&T) -> bool) -> Option<T>",
            "and (Option<U>) -> Option<U>",
            "and_then ((T) -> Option<U>) -> Option<U>",
            "or (Option<T>) -> Option<T>",
            "or_else (() -> Option<T>) -> Option<T>",
          ],
        },
      ],
    },
    {
      type: "Option<&T>",
      path: "std/option/enum.Option.html",
      groups: [
        {
          comment: "Cloning inner",
          items: [
            "cloned () -> Option<T> where T: Clone",
          ],
        },
      ],
    },
    {
      type: "Option<Result<T, E>>",
      path: "std/option/enum.Option.html",
      groups: [
        {
          comment: "Transposing",
          items: [
            "transpose () -> Result<Option<T>, E>",
          ],
        }
      ],
    },
    {
      type: "&Option<T>",
      path: "std/option/enum.Option.html",
      groups: [
        {
          comment: "Checking inner",
          items: [
            "is_some () -> bool",
            "is_none () -> bool",
          ],
        },
        {
          comment: "To inner reference",
          items: [
            "as_ref () -> Option<&T>",
            "iter () -> Iterator<&T>",
          ],
        },
      ],
    },
    {
      type: "&mut Option<T>",
      path: "std/option/enum.Option.html",
      groups: [
        {
          comment: "To inner mutable reference",
          items: [
            "as_mut () -> Option<&mut T>",
            "iter_mut () -> Iterator<&mut T>",
          ],
        },
        {
          comment: "Mutation",
          items: [
            "take () -> Option<T>",
            "replace (T) -> Option<T>",
            "get_or_insert (T) -> &mut T",
            "get_or_insert_with (() -> T) -> &mut T",
          ],
        },
      ],
    },
  ],
  [
    {
      type: "Result<T, E>",
      path: "std/result/enum.Result.html",
      groups: [
        {
          comment: "To inner type",
          items: [
            "unwrap () -> T where E: Debug",
            "unwrap_err () -> E where T: Debug",
            "unwrap_or (T) -> T",
            "unwrap_or_else ((E) -> T) -> T",
            "unwrap_or_default () -> T where T: Default",
            "expect (&str) -> T",
            "expect_err (&str) -> E",
            "ok () -> Option<T>",
            "err () -> Option<E>",
          ],
        },
        {
          comment: "Mapping",
          items: [
            "map ((T) -> U) -> Result<U, E>",
            "map_err ((E) -> F) -> Result<T, F>",
          ],
        },
        {
          comment: "Conditioning",
          items: [
            "and (Result<U, E>) -> Result<U, E>",
            "and_then ((T) -> Result<U, E>) -> Result<U, E>",
            "or (Result<T, F>) -> Result<T, F>",
            "or_else ((E) -> Result<T, F>) -> Result<T, F>",
          ],
        },
      ],
    },
    {
      type: "Result<Option<T>, E>",
      path: "std/result/enum.Result.html",
      groups: [
        {
          comment: "Transposing",
          items: [
            "transpose () -> Option<Result<T, E>>",
          ],
        }
      ],
    },
    {
      type: "&Result<T, E>",
      path: "std/result/enum.Result.html",
      groups: [
        {
          comment: "Checking inner",
          items: [
            "is_ok () -> bool",
            "is_err () -> bool",
          ],
        },
        {
          comment: "To inner reference",
          items: [
            "as_ref () -> Result<&T, &E>",
            "iter () -> Iterator<Item = &T>",
          ],
        },
      ],
    },
    {
      type: "&mut Result<T, E>",
      path: "std/result/enum.Result.html",
      groups: [
        {
          comment: "To inner mutable reference",
          items: [
            "as_mut () -> Result<&mut T, &mut E>",
            "iter_mut () -> Iterator<Item = &mut T>",
          ],
        },
      ],
    },
  ],
];

const BASE_URL = "https://doc.rust-lang.org/";

const words = new Map();
for (const [kind, names] of [
  ["trait", [
    ["Clone", "std/clone"],
    ["Debug", "std/fmt"],
    ["Default", "std/default"],
    ["DoubleEndedIterator", "std/iter"],
    ["ExactSizeIterator", "std/iter"],
    ["Extend", "std/iter"],
    ["FromIterator", "std/iter"],
    ["IntoIterator", "std/iter"],
    ["Iterator", "std/iter"],
    ["Ord", "std/cmp"],
    ["PartialEq", "std/cmp"],
    ["PartialOrd", "std/cmp"],
    ["Product", "std/iter"],
    ["RangeBounds", "std/ops"],
    ["Sum", "std/iter"],
  ]],
  ["enum", [
    ["Option", "std/option"],
    ["Ordering", "std/cmp"],
    ["Result", "std/result"],
  ]],
  ["primitive", [
    "&",
    ["bool", "std"],
    "mut",
    ["str", "std"],
    ["usize", "std"],
  ]],
  ["struct", [
    ["Range", "std/ops"],
  ]],
  ["type", ["Item"]],
  ["where", ["where"]],
]) {
  for (const name of names) {
    let word, path;
    if (typeof name === 'string') {
      word = name;
    } else {
      [word, path] = name;
      path += `/${kind}.${word}.html`;
    }
    words.set(word, { kind, path });
  }
}

function build() {
  const main = $c('main');
  for (const superGroup of data) {
    const section = $c('section');
    for (const { mod, type, path, groups } of superGroup) {
      const h2 = $c('h2');
      const a = $c('a', type || mod);
      a.href = `${BASE_URL}${path}`;
      h2.appendChild(a);
      section.appendChild(h2);
      for (const { comment, items } of groups) {
        section.appendChild($c('h3', comment));
        const ul = $c('ul');
        for (const item of items) {
          ul.appendChild(generateItem(item, path, type ? 'method' : 'fn'));
        }
        section.appendChild(ul);
      }
    }
    main.appendChild(section);
  }
  document.body.appendChild(main);
}

function generateItem(item, base, kind) {
  const li = $c('li');
  const pieces = item.split(/([^\w&()]+|[&()])/);
  const fn = pieces.shift();
  const a = $c('a', fn, kind);
  if (kind === 'method') {
    a.href = `${BASE_URL}${base}#method.${fn}`;
  } else if (kind === 'fn') {
    a.href = `${BASE_URL}${base}fn.${fn}.html`;
  }
  li.appendChild(a);
  const levels = [li];
  for (const piece of pieces) {
    if (piece === ')') {
      const nested = levels.shift();
      levels[0].appendChild(nested);
    }
    const info = words.get(piece);
    if (info) {
      const { kind, path } = info;
      const element = $c(path ? 'a' : 'span', piece, kind);
      if (path) {
        element.href = BASE_URL + path;
      }
      levels[0].appendChild(element);
    } else {
      levels[0].appendChild(document.createTextNode(piece));
    }
    if (piece === '(') {
      const nested = $c('span', undefined, 'nested');
      levels.unshift(nested);
    }
  }
  li.normalize();
  return li;
}

function $c(tag, text, className) {
  const element = document.createElement(tag);
  if (text) {
    element.textContent = text;
  }
  if (className) {
    element.className = className;
  }
  return element;
}

window.addEventListener("DOMContentLoaded", build);
