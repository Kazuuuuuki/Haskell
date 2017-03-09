instance F.Fordable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x          `mappend`
                           F,foldMap f r