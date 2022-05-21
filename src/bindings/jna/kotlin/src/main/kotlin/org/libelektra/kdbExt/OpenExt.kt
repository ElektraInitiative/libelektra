package org.libelektra.kdbExt

import org.libelektra.KDB

fun withKDB(callback: KDB.() -> Unit) {
    KDB.open().use {
        it.callback()
    }
}

