(function () {
    const handledKeys = new Set([
        "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight",
        "Up", "Down", "Left", "Right",
        "w", "a", "s", "d", "l", "f", "i", "t", "q",
        "F5", "F9", " ", "Escape", "Enter", "Backspace",
        "Home", "PageUp", "End", "PageDown"
    ]);

    function focusGame() {
        window.setTimeout(function () {
            const shell = document.querySelector(".spelunk-shell");
            if (shell) {
                shell.focus();
            }
        }, 0);
    }

    function handlePointerDown() {
        focusGame();
    }

    function handleKeyDown(event) {
        const key = event.key;
        const normalized = key && key.length === 1 ? key.toLowerCase() : key;
        const isDigit = normalized >= "0" && normalized <= "9";

        if (!handledKeys.has(key) && !handledKeys.has(normalized) && !isDigit) {
            return;
        }

        event.preventDefault();

        if (window.spelunk.keyboardReceiver) {
            window.spelunk.keyboardReceiver.invokeMethodAsync("HandleKeyDown", key)
                .catch(function (error) { console.error("spelunk key dispatch failed", error); });
        }
    }

    function installKeyboard() {
        if (window.__spelunkPointerHandler) {
            document.removeEventListener("pointerdown", window.__spelunkPointerHandler, true);
        }

        if (window.__spelunkKeyHandler) {
            document.removeEventListener("keydown", window.__spelunkKeyHandler, true);
        }

        window.__spelunkPointerHandler = handlePointerDown;
        window.__spelunkKeyHandler = handleKeyDown;

        document.addEventListener("pointerdown", window.__spelunkPointerHandler, true);
        document.addEventListener("keydown", window.__spelunkKeyHandler, true);
    }
    
   */ window.spelunk = {
        keyboardReceiver: window.spelunk && window.spelunk.keyboardReceiver || null,
        setKeyboardReceiver: function (receiver) {
            window.spelunk.keyboardReceiver = receiver;
            installKeyboard();
        },
        installKeyboard: installKeyboard,
        focusGame: focusGame
    };

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", installKeyboard, { once: true });
    } else {
        installKeyboard();
    }
}());

Extensions to Existing Project

SB
Fix graphics (hard)
make into true compiler

Fishy
Everything that is in Stockfish but not in Fishy
EUNN

9 Mens Morris
Everything that is in Fishy but not here
Everything that is in Stockfish but not here
Improve GUI

QuillToDocx
Nothing

Spelunking
Everything that is in Moria but not here
Everything that is in Angband but not Moria
All on web

MusicCatalog
Improve search options
Speed up loading
Normalize database

New Projects

Modula2 compiler
squiggle free C#
Ad free downloader