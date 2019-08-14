package org.libelektra.exception.mapper;

import org.libelektra.Key;
import org.libelektra.exception.*;

public class ExceptionMapperService {

	public static KDBException getMappedException(Key k) {

		// At least one error or warning should be there
		if (k.getMeta("error/number").isNull() && k.getMeta("warnings/#00/number").isNull()) {
			Key temporaryError = Key.create("user/temporary/errorkey");
			temporaryError.setMeta("error/number", InternalException.errorNumber());
			temporaryError.setMeta("error/reason", "Sorry, Elektra returned an error/warning but no " +
					"information along it, please report this incident at https://issues.libelektra.org/");
			return new InternalException(temporaryError);
		}

		String errorNumber = k.getMeta("error/number").getString();

		if (errorNumber.equals(ResourceException.errorNumber())) {
			return new ResourceException(k);
		} else if (errorNumber.equals(OutOfMemoryException.errorNumber())) {
			return new OutOfMemoryException(k);
		} else if (errorNumber.equals(InternalException.errorNumber())) {
			return new InternalException(k);
		} else if (errorNumber.equals(InterfaceException.errorNumber())) {
			return new InterfaceException(k);
		} else if (errorNumber.equals(InstallationException.errorNumber())) {
			return new InstallationException(k);
		} else if (errorNumber.equals(PluginMisbehaviorException.errorNumber())) {
			return new PluginMisbehaviorException(k);
		} else if (errorNumber.equals(ConflictingStateException.errorNumber())) {
			return new ConflictingStateException(k);
		} else if (errorNumber.equals(SyntacticValidationException.errorNumber())) {
			return new SyntacticValidationException(k);
		} else if (errorNumber.equals(SemanticValidationException.errorNumber())) {
			return new SemanticValidationException(k);
		} else {
			Key temporaryError = Key.create("user/temporary/errorkey");
			temporaryError.setMeta("error/number", InternalException.errorNumber());
			temporaryError.setMeta("error/reason", "Sorry, could not map error number '" +
					errorNumber + "'. Please report this incident at https://issues.libelektra.org/");
			return new InternalException(temporaryError);
		}

	}

}
