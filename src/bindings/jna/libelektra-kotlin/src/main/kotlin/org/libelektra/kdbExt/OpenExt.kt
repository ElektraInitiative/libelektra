package org.libelektra.kdbExt

import org.libelektra.KDB

/**
 * This method allows to interact with KDB in the defined scope of the callback function
 *
 * @param callback function which interacts with KDB
 */
fun withKDB(callback: KDB.() -> Unit) {
    KDB.open().use {
        it.callback()
    }
}

