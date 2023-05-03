/**
 * @file
 *
 * @brief Header for array specifications in spec plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ARRAY_SPEC_H
#define ARRAY_SPEC_H

#include <elektra/core/types.h>

#include <stdbool.h>

char * createArrayElementName (int arrayNumber);
char * createFormattedArrayKeyNameInDefaultNamespace (char * keyNameWithoutNamespace, int arrayNumber, int pos);
void instantiateArraySpecificationAndCopyMeta (Key * specKey, KeySet * ks, int arraySize, int pos);
void setArrayPositions (const char * keyNameWithoutNamespace, int * arrayPositions, int arraySize);
int copyAllMetaDataForMatchingArrayKeyName (KeySet * ks, Key * parentKey, Key * specKey, bool isKdbGet);

Key * getMatchingKeyFromKeySet (KeySet * ks, char * name);
Key * getArraySizeOfArrayParent (KeySet * specKeys, Key * specKey);
int getNumberOfArrayCharactersInSpecName (Key * specKey);
int getActualArraySize (KeySet * ks, Key * specKey, int arrayPosition);

bool containsArraySpecElementWithNoDigitOrUnderlineAfterwards (const char * keyNameWithNamespace);
bool isArraySpecification (Key * specKey);
bool containsUnderlineInArraySpec (Key * specKey);
bool validateArraySize (Key * key, Key * specKey);
bool isValidArraySize (KeySet * ks, KeySet * specKeys, Key * specKey);

#endif
