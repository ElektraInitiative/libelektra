/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests.hpp>

#include <elektradiff.hpp>

TEST (diff, calculateDiff)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/hello", KEY_END));
	ksOld.append (Key ("user:/goodbye", KEY_END));
	ksOld.append (Key ("user:/greeting", KEY_VALUE, "world", KEY_END));

	KeySet ksNew;
	ksNew.append (Key ("user:/hello", KEY_END));
	ksNew.append (Key ("user:/greeting", KEY_VALUE, "elektra", KEY_END));
	ksNew.append (Key ("user:/new", KEY_END));

	// Act
	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, "/");

	// Assert
	EXPECT_TRUE (diff.getRemovedKeys ().lookup ("user:/goodbye").isValid ());
	EXPECT_TRUE (diff.getAddedKeys ().lookup ("user:/new").isValid ());
	EXPECT_TRUE (diff.getModifiedKeys ().lookup ("user:/greeting").isValid ());
}

TEST (diff, meta)
{
	// Arrange
	KeySet ksOld;
	Key oldKey ("user:/hello", KEY_END);
	ksOld.append (oldKey);
	oldKey.setMeta ("meta:/same", "123");
	oldKey.setMeta ("meta:/modified", "123");
	oldKey.setMeta ("meta:/removed", "123");

	KeySet ksNew;
	Key newKey ("user:/hello", KEY_END);
	ksNew.append (newKey);
	newKey.setMeta ("meta:/same", "123");
	newKey.setMeta ("meta:/modified", "456");
	newKey.setMeta ("meta:/added", "123");

	// Act
	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, "/");

	// Assert
	EXPECT_TRUE (diff.getAddedMetaKeys (newKey).lookup ("meta:/added").isValid ());
	EXPECT_TRUE (diff.getRemovedMetaKeys (newKey).lookup ("meta:/removed").isValid ());
	EXPECT_TRUE (diff.getModifiedMetaKeys (newKey).lookup ("meta:/modified").isValid ());
}

TEST (diff, cut)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/test1/a", KEY_END));
	ksOld.append (Key ("user:/test1/b", KEY_END));
	ksOld.append (Key ("user:/test2/a", KEY_END));
	ksOld.append (Key ("user:/test2/b", KEY_END));

	KeySet ksNew;
	ksNew.append (Key ("user:/test1/a", KEY_END));
	ksNew.append (Key ("user:/test2/a", KEY_END));

	Key parentKey;

	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	// Act
	ElektraDiff diff2 = diff.cut ("user:/test2");

	// Assert
	EXPECT_TRUE (diff.getRemovedKeys ().lookup ("user:/test1/b").isValid ());
	EXPECT_FALSE (diff.getRemovedKeys ().lookup ("user:/test2/b").isValid ());
	EXPECT_TRUE (diff2.getRemovedKeys ().lookup ("user:/test2/b").isValid ());
	EXPECT_FALSE (diff2.getRemovedKeys ().lookup ("user:/test1/b").isValid ());
}

TEST (diff, removeOther)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/test1/a", KEY_END));
	ksOld.append (Key ("user:/test1/b", KEY_END));
	ksOld.append (Key ("user:/test2/a", KEY_END));
	ksOld.append (Key ("user:/test2/b", KEY_END));

	KeySet ksNew;
	ksNew.append (Key ("user:/test1/a", KEY_END));
	ksNew.append (Key ("user:/test2/a", KEY_END));

	Key parentKey;

	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	// Act
	diff.removeOther ("user:/test2");

	// Assert
	EXPECT_TRUE (diff.getRemovedKeys ().lookup ("user:/test2/b").isValid ());
	EXPECT_FALSE (diff.getRemovedKeys ().lookup ("user:/test1/b").isValid ());
}

TEST (diff, removeSameOrBelow)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/test1/a", KEY_END));
	ksOld.append (Key ("user:/test1/b", KEY_END));
	ksOld.append (Key ("user:/test2/a", KEY_END));
	ksOld.append (Key ("user:/test2/b", KEY_END));

	KeySet ksNew;
	ksNew.append (Key ("user:/test1/a", KEY_END));
	ksNew.append (Key ("user:/test2/a", KEY_END));

	Key parentKey;

	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	// Act
	diff.removeSameOrBelow ("user:/test2");

	// Assert
	EXPECT_TRUE (diff.getRemovedKeys ().lookup ("user:/test1/b").isValid ());
	EXPECT_FALSE (diff.getRemovedKeys ().lookup ("user:/test2/b").isValid ());
}

TEST (diff, assign)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/hello", KEY_END));
	ksOld.append (Key ("user:/goodbye", KEY_END));

	KeySet ksNew;
	ksNew.append (Key ("user:/hello", KEY_END));

	Key parentKey;

	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	// Act & Assert
	ElektraDiff diffAssign1;
	diffAssign1 = diff;
	EXPECT_EQ (diff.getReferenceCounter (), 2);

	ElektraDiff diffAssign2;
	diffAssign2 = diffAssign1;
	EXPECT_EQ (diff.getReferenceCounter (), 3);

	diffAssign1 = ElektraDiff ();
	EXPECT_EQ (diff.getReferenceCounter (), 2);
}

TEST (diff, isEmpty)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/hello", KEY_END));
	ksOld.append (Key ("user:/goodbye", KEY_END));

	KeySet ksNew;
	ksNew.append (Key ("user:/hello", KEY_END));

	Key parentKey;

	ElektraDiff diffWithDifferences = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);
	ElektraDiff diffWithoutDifferences = ElektraDiff::calculateDiff (ksOld, ksOld, parentKey);

	// Act & Assert
	EXPECT_EQ (diffWithDifferences.isEmpty (), false);
	EXPECT_EQ (diffWithoutDifferences.isEmpty (), true);
}

TEST (diff, dup)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/test1/a", KEY_END));
	ksOld.append (Key ("user:/test1/b", KEY_END));
	ksOld.append (Key ("user:/test2/a", KEY_END));
	ksOld.append (Key ("user:/test2/b", KEY_END));

	KeySet ksNew;
	ksNew.append (Key ("user:/test1/a", KEY_END));
	ksNew.append (Key ("user:/test2/a", KEY_END));

	Key parentKey;

	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	// Act
	ElektraDiff second = diff.dup ();

	// Assert
	EXPECT_TRUE (diff.getDiff () != nullptr);
	EXPECT_TRUE (second.getDiff () != nullptr);

	EXPECT_EQ (diff.getReferenceCounter (), 1);
	EXPECT_EQ (second.getReferenceCounter (), 1);
}
