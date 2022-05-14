package org.libelektra.keyExt

import org.libelektra.Key

fun Key.isEmpty() = string.isEmpty()

fun Key.isNotEmpty() = !isEmpty()
