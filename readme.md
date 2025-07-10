# 🦎 Luminify

## 🚀 Description

**Luminify** est un outil de minification de code **Lua 5.3**, écrit en **OCaml**.  
Il réduit la taille des scripts Lua en supprimant les espaces inutiles, les commentaires, et en réécrivant certaines expressions pour produire un code plus compact et plus rapide à charger.

---

## 💡 Pourquoi ?

- 📦 Minimiser la taille des scripts distribués.
- ⚡ Améliorer les temps de chargement et la performance.
- ✨ Expérimenter et démontrer la puissance d’OCaml sur un projet concret.

---

## 📄 Documentation Lua utilisée

Ce projet se base sur la documentation officielle de Lua 5.3 :  
👉 [Lua 5.3 Reference Manual](https://www.lua.org/manual/5.3/manual.html)

---

## 🔧 Fonctionnalités prévues

- Suppression des espaces et indentations superflus.
- Suppression des commentaires.
- Réécriture optionnelle des noms de variables temporaires.
- Préservation complète de la sémantique du code original.

---

## ⚙️ Compilation

### Avec `make`

```bash
make && ./bin/main
```

## 📝 Licence

Ce projet est open source (MIT).