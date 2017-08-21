### Vault

A typed, persistent store for values of arbitrary types. This is a port of the haskell [Vault](https://github.com/HeinrichApfelmus/vault) library Heinrich Apfelmus.

Think of it as ```Map``` that can store for values of arbitrary types, but it's type-safe.
You can store of any types by creating ```Key a``` first, this key used to parameterized the
type you are going to store in vault.

In summary a vault API:

```haskell
data Vault
data Key a

newKey :: forall e a. Eff (ref :: REF | e) (Key a)
empty  :: Vault
lookup :: forall a. Key a -> Vault -> Maybe a
insert :: forall a. Key a -> a -> Vault -> Vault
delete :: forall a. Key a -> Vault -> Vault
```
