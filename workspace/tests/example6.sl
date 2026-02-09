forall a b . func map (f : (a) -> b, v : a[]) : b[] {
    let result = new b[v.size];
    for (i = 0; i < v.size ; i++) {
      result[i] = f(v[i]);
    }
    return result;
}