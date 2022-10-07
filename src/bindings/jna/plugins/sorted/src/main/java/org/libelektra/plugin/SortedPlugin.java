package org.libelektra.plugin;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import org.libelektra.*;

public class SortedPlugin implements Plugin {

  private static final String PLUGIN_NAME = "sorted";
  private static final String META_SORTED = "meta:/check/sorted";
  private static final String META_SORTED_DIRECTION = "meta:/check/sorted/direction";

  private enum Direction {
    ASC,
    DESC
  }

  @Nonnull
  @Override
  public String getName() {
    return PLUGIN_NAME;
  }

  @Override
  public int open(KeySet config, Key errorKey) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int get(KeySet keySet, Key parentKey) {
    if (isPluginContractRequested(parentKey)) {
      SortedContract.appendAllTo(keySet);
      return STATUS_SUCCESS;
    }

    hasSortingErrors(keySet, parentKey::addWarning);

    return STATUS_SUCCESS;
  }

  private boolean isPluginContractRequested(Key parentKey) {
    return parentKey.isBelowOrSame(Key.create(PROCESS_CONTRACT_ROOT));
  }

  @Override
  public int set(KeySet keySet, Key parentKey) throws KDBException {
    return hasSortingErrors(keySet, parentKey::setError) ? STATUS_ERROR : STATUS_SUCCESS;
  }

  /**
   * Checks whether the sorting of the array specified by the meta values has errors or not.
   *
   * @param keySet Key set to check for errors
   * @param addErrorFunction Function for handling encountered errors
   * @return true if the sorting defined by the meta values is invalid. Otherwise, false.
   */
  private boolean hasSortingErrors(
      KeySet keySet, BiFunction<ErrorCode, String, Key> addErrorFunction) {
    AtomicBoolean foundError = new AtomicBoolean(false);

    keySet.forEach(
        key -> {
          Optional<ReadableKey> sortedMeta = key.getMeta(META_SORTED);

          if (sortedMeta.isPresent()) {
            String sortedKeyValue = sortedMeta.get().getString();

            Direction direction = Direction.ASC;
            try {
              direction = getDirection(key);
            } catch (IllegalArgumentException e) {
              var invalidDirection = e.getMessage().substring(e.getMessage().lastIndexOf("."));
              addErrorFunction.apply(
                  ErrorCode.VALIDATION_SEMANTIC,
                  "Invalid direction for sorted plugin in key '"
                      + key.getName()
                      + "':"
                      + invalidDirection);
              foundError.set(true);
            }

            List<Key> arrayKeys = getSortedArrayKeys(keySet, key, direction);

            if (!isSorted(arrayKeys, key.getName() + "/#_*\\d*" + sortedKeyValue)) {
              addErrorFunction.apply(
                  ErrorCode.VALIDATION_SEMANTIC,
                  "Values are not sorted below key '" + key.getName() + "'");
              foundError.set(true);
            }
          }
        });
    return foundError.get();
  }

  private boolean isSorted(List<Key> arrayKeys, String pathRegex) {

    List<Key> filtered =
        arrayKeys.stream()
            .filter(it -> it.getName().matches(pathRegex))
            .collect(Collectors.toList());

    return filtered.stream()
        .sorted(Comparator.comparing(ReadableKey::getString))
        .collect(Collectors.toList())
        .equals(filtered);
  }

  private List<Key> getSortedArrayKeys(KeySet keySet, Key parentKey, Direction direction) {
    return keySet.stream()
        .filter(it -> it.getName().startsWith(parentKey.getName() + "/#"))
        .sorted(
            (o1, o2) -> {
              int index1 = parseArrayIndex(o1.getName(), parentKey.getName());
              int index2 = parseArrayIndex(o2.getName(), parentKey.getName());

              if (index1 == index2) return 0;

              switch (direction) {
                case ASC:
                  return index1 < index2 ? -1 : 1;
                case DESC:
                  return index1 > index2 ? -1 : 1;
                default:
                  throw new IllegalStateException(
                      "Enum switch reached illegal default state, was a new Direction added?");
              }
            })
        .collect(Collectors.toList());
  }

  private int parseArrayIndex(String key, String prefix) {
    return Integer.parseInt(
        key.substring(prefix.length() + 1).split("/")[0].replaceAll("#", "").replaceAll("_", ""));
  }

  private Direction getDirection(Key key) throws IllegalArgumentException {
    Optional<ReadableKey> sortedDirectionMeta = key.getMeta(META_SORTED_DIRECTION);
    Direction direction = Direction.ASC;

    if (sortedDirectionMeta.isPresent()) {
      String sortedDirectionKey = sortedDirectionMeta.get().getString();

      return Direction.valueOf(sortedDirectionKey.toUpperCase());
    }

    return direction;
  }

  @Override
  public int error(KeySet keySet, Key parentKey) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int close(Key parentKey) {
    throw new UnsupportedOperationException();
  }
}
