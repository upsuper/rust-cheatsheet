const data = [
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
            "cmp (IntoIterator<Item = T>) -> Ordering where T: Ord",
            "partial_cmp (IntoIterator<Item = T>)\n-> Option<Ordering> where T: PartialOrd",
          ],
        },
        {
          comment: "Reversing and cycling",
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
        {
          comment: "Try iterating",
          items: [
            "try_for_each ((T) -> Option<()>)    -> Option<()>",
            "try_for_each ((T) -> Result<(), E>) -> Result<(), E>",
            "try_fold (S, (S, T) -> Option<S>)    -> Option<S>",
            "try_fold (S, (S, T) -> Result<S, E>) -> Result<S, E>",
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
          comment: "Matching",
          items: [
            "contains    (&T)   -> bool where T: PartialEq",
            "starts_with (&[T]) -> bool where T: PartialEq",
            "ends_with   (&[T]) -> bool where T: PartialEq",
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
            "get (usize | RangeBounds<usize>) -> Option<&T>",
            "iter () -> Iterator<Item = &T>",
          ],
        },
        {
          comment: "Length",
          items: [
            "len () -> usize",
            "is_empty () -> bool",
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
          comment: "Overriding",
          items: [
            "swap_with_slice  (&mut [T])",
            "copy_from_slice  (&[T]) where T: Copy",
            "clone_from_slice (&[T]) where T: Clone",
          ],
        },
        {
          comment: "Getting and iterating",
          items: [
            "first_mut () -> Option<&mut T>",
            "last_mut  () -> Option<&mut T>",
            "get_mut (usize | RangeBounds<usize>) -> Option<&mut T>",
            "iter_mut () -> Iterator<Item = &mut T>",
          ],
        },
      ],
    },
    {
      type: "&mut Vec<T>",
      path: "std/vec/struct.Vec.html",
      groups: [
        {
          comment: "Adding and removing single item",
          items: [
            "push (T)",
            "pop () -> Option<T>",
            "insert (usize, T)",
            "remove (usize) -> T",
            "swap_remove (usize) -> T",
          ],
        },
        {
          comment: "Extending",
          items: [
            "append (&mut Vec<T>)",
            "Extend<T>::extend (IntoIterator<Item = T>)",
            "Extend<&'a T>::extend (IntoIterator<Item = &T>) where T: Copy",
            "extend_from_slice (&[T]) where T: Clone",
          ],
        },
        {
          comment: "Resizing",
          items: [
            "truncate (usize)",
            "resize (usize, T) where T: Clone",
            "resize_with (usize, () -> T)",
          ],
        },
        {
          comment: "Clearing",
          items: [
            "clear ()",
            "retain ((&T) -> bool)",
          ],
        },
        {
          comment: "Removing or replacing range into iterator",
          items: [
            "drain  (RangeBounds<usize>) -> Iterator<T>",
            "splice (RangeBounds<usize>, IntoIterator<Item = T>) -> Iterator<T>",
          ],
        },
        {
          comment: "Deduplicating",
          items: [
            "dedup () where T: PartialEq",
            "dedup_by ((&mut T, &mut T) -> bool)",
            "dedup_by_key ((&mut T) -> K) where K: PartialEq",
          ],
        },
        {
          comment: "Splitting off",
          items: [
            "split_off (usize) -> Vec<T>",
          ],
        },
        {
          comment: "Capacity manipulation",
          items: [
            "reserve (usize)",
            "reserve_exact (usize)",
            "shrink_to_fit ()",
          ],
        },
      ],
    },
  ],
  [
    {
      type: "&[u8]",
      path: "std/primitive.slice.html",
      groups: [
        {
          comment: "ASCII",
          items: [
            "is_ascii () -> bool",
            "eq_ignore_ascii_case (&[u8]) -> bool",
            "to_ascii_uppercase () -> Vec<u8>",
            "to_ascii_lowercase () -> Vec<u8>",
          ],
        },
      ],
    },
    {
      type: "&mut [u8]",
      path: "std/primitive.slice.html",
      groups: [
        {
          comment: "ASCII",
          items: [
            "make_ascii_uppercase ()",
            "make_ascii_lowercase ()",
          ],
        }
      ],
    },
    {
      type: "&str",
      path: "std/primitive.str.html",
      groups: [
        {
          comment: "Chars",
          items: [
            "chars () -> Iterator<Item = char>",
            "char_indices () -> Iterator<Item = (usize, char)>",
            "is_char_boundary (usize) -> bool",
          ],
        },
        {
          comment: "Bytes",
          items: [
            "bytes () -> Iterator<Item = u8>",
            "as_bytes () -> &[u8]",
          ],
        },
        {
          comment: "Splitting to two parts",
          items: [
            "split_at (usize) -> (&str, &str)",
          ],
        },
        {
          comment: "Splitting to iterator",
          items: [
            "lines () -> Iterator<Item = &str>",
            "split_whitespace () -> Iterator<Item = &str>",
            "split_ascii_whitespace () -> Iterator<Item = &str>",
            "split  (char | &str | &[char] | (char) -> bool) -> Iterator<Item = &str>",
            "rsplit (char | &str | &[char] | (char) -> bool) -> Iterator<Item = &str>",
            "splitn  (usize, char | &str | &[char] | (char) -> bool) -> Iterator<Item = &str>",
            "rsplitn (usize, char | &str | &[char] | (char) -> bool) -> Iterator<Item = &str>",
            "split_terminator  (char | &str | &[char] | (char) -> bool) -> Iterator<Item = &str>",
            "rsplit_terminator (char | &str | &[char] | (char) -> bool) -> Iterator<Item = &str>",
          ],
        },
        {
          comment: "Trimming",
          items: [
            "trim       () -> &str",
            "trim_start () -> &str",
            "trim_end   () -> &str",
            "trim_matches       (char | &str | &[char] | (char) -> bool) -> &str",
            "trim_start_matches (char | &str | &[char] | (char) -> bool) -> &str",
            "trim_end_matches   (char | &str | &[char] | (char) -> bool) -> &str",
          ],
        },
        {
          comment: "Matching and finding",
          items: [
            "contains    (char | &str | &[char] | (char) -> bool) -> bool",
            "starts_with (char | &str | &[char] | (char) -> bool) -> bool",
            "ends_with   (char | &str | &[char] | (char) -> bool) -> bool",
            "find  (char | &str | &[char] | (char) -> bool) -> Option<usize>",
            "rfind (char | &str | &[char] | (char) -> bool) -> Option<usize>",
            "matches  (char | &str | &[char] | (char) -> bool) -> Iterator<Item = &str>",
            "rmatches (char | &str | &[char] | (char) -> bool) -> Iterator<Item = &str>",
            "match_indices  (char | &str | &[char] | (char) -> bool) -> Iterator<Item = (usize, &str)>",
            "rmatch_indices (char | &str | &[char] | (char) -> bool) -> Iterator<Item = (usize, &str)>",
          ],
        },
        {
          comment: "Case",
          items: [
            "to_uppercase () -> String",
            "to_lowercase () -> String",
            "to_ascii_uppercase () -> String",
            "to_ascii_lowercase () -> String",
            "eq_ignore_ascii_case (&str) -> bool",
          ],
        },
        {
          comment: "Replacing",
          items: [
            "replace  (char | &str | &[char] | (char) -> bool, &str) -> String",
            "replacen (char | &str | &[char] | (char) -> bool, &str, usize) -> String",
          ],
        },
        {
          comment: "Length",
          items: [
            "len () -> usize",
            "is_empty () -> bool",
          ],
        },
        {
          comment: "Misc.",
          items: [
            "is_ascii () -> bool",
            "repeat (usize) -> String",
            "encode_utf16 () -> Iterator<Item = u16>",
            "parse () -> Result<F, E> where F: FromStr",
          ],
        },
      ],
    },
    {
      type: "&mut str",
      path: "std/primitive.str.html",
      groups: [
        {
          comment: "Splitting to two parts",
          items: [
            "split_at_mut (usize) -> (&mut str, &mut str)",
          ],
        },
        {
          comment: "Case conversion",
          items: [
            "make_ascii_uppercase ()",
            "make_ascii_lowercase ()",
          ],
        },
      ],
    },
    {
      type: "&mut String",
      path: "std/string/struct.String.html",
      groups: [
        {
          comment: "Inserting and appending string",
          items: [
            "push_str (&str)",
            "insert_str (usize, &str)",
          ],
        },
        {
          comment: "Adding and removing char",
          items: [
            "push (char)",
            "pop () -> Option<char>",
            "insert (usize, char)",
            "remove (usize) -> char",
          ],
        },
        {
          comment: "Clearing",
          items: [
            "clear ()",
            "truncate (usize)",
            "retain ((char) -> bool)",
          ],
        },
        {
          comment: "Capacity manipulation",
          items: [
            "reserve (usize)",
            "reserve_exact (usize)",
            "shrink_to_fit ()",
          ],
        },
        {
          comment: "Misc.",
          items: [
            "split_off (usize) -> String",
            "replace_range (RangeBounds<usize>, &str)",
            "drain (RangeBounds<usize>) -> Iterator<Item = char>",
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
    ["Copy", "std/marker"],
    ["Debug", "std/fmt"],
    ["Default", "std/default"],
    ["DoubleEndedIterator", "std/iter"],
    ["ExactSizeIterator", "std/iter"],
    ["Extend", "std/iter"],
    ["FromIterator", "std/iter"],
    ["FromStr", "std/str"],
    ["IntoIterator", "std/iter"],
    ["Iterator", "std/iter"],
    ["Ord", "std/cmp"],
    ["PartialEq", "std/cmp"],
    ["PartialOrd", "std/cmp"],
    ["Product", "std/iter"],
    ["RangeBounds", "std/ops"],
    ["String", "std/string"],
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
    ["char", "std"],
    "mut",
    ["str", "std"],
    ["u8", "std"],
    ["u16", "std"],
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
  const main = document.querySelector('main');
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
}

function generateItem(item, base, kind) {
  const li = $c('li');
  // Handle trait
  const colonsPos = item.indexOf('::');
  const hasTrait = colonsPos > 0;
  const trait = hasTrait ? item.slice(0, colonsPos) : null;
  item = hasTrait ? item.slice(colonsPos + 2) : item;
  // Handle method name
  const firstSpace = item.indexOf(' ');
  const funcName = item.slice(0, firstSpace);
  const a = $c('a', funcName, kind);
  if (kind === 'method') {
    let hash;
    if (trait) {
      hash = `impl-${escape(trait)}`;
    } else {
      hash = `method.${funcName}`;
    }
    a.href = `${BASE_URL}${base}#${hash}`;
  } else if (kind === 'fn') {
    a.href = `${BASE_URL}${base}fn.${funcName}.html`;
  }
  li.appendChild(a);
  // Format the rest
  const pieces = item.slice(firstSpace).split(/([^\w&()]+|[&()])/);
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
