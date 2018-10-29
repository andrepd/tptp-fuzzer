type 'a range = {
  lower: 'a;
  upper: 'a;
}

type 'a t = 'a range

let (--) lower upper = {lower; upper}

let point x = x--x
