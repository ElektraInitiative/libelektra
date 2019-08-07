package org.libelektra.exception.mapper;

import org.libelektra.Key;
import org.libelektra.exception.*;

public class ExceptionMapperService {

	public static KDBException getMappedException(Key k) {

		// At least one error or warning should be there
		if (k.getMeta("error/number").isNull() && k.getMeta("warnings/#00/number").isNull()) {
			Key temporaryError = Key.create("user/temporary/errorkey");
			temporaryError.setMeta("error/number", InternalException.errorCode());
			temporaryError.setMeta("error/reason", "Sorry, Elektra returned an error/warning but no " +
					"information along it, please report this incident at https://issues.libelektra.org/");
			return new InternalException(temporaryError);
		}

		String errorCode = k.getMeta("error/number").getString();

		if (errorCode.equals(ResourceException.errorCode())) {
			return new ResourceException(k);
		} else if (errorCode.equals(OutOfMemoryException.errorCode())) {
			return new OutOfMemoryException(k);
		} else if (errorCode.equals(InternalException.errorCode())) {
			return new InternalException(k);
		} else if (errorCode.equals(InterfaceException.errorCode())) {
			return new InterfaceException(k);
		} else if (errorCode.equals(InstallationException.errorCode())) {
			return new InstallationException(k);
		} else if (errorCode.equals(PluginMisbehaviorException.errorCode())) {
			return new PluginMisbehaviorException(k);
		} else if (errorCode.equals(ConflictingStateException.errorCode())) {
			return new ConflictingStateException(k);
		} else if (errorCode.equals(SyntacticValidationException.errorCode())) {
			return new SyntacticValidationException(k);
		} else if (errorCode.equals(SemanticValidationException.errorCode())) {
			return new SemanticValidationException(k);
		} else {
			Key temporaryError = Key.create("user/temporary/errorkey");
			temporaryError.setMeta("error/number", InternalException.errorCode());
			temporaryError.setMeta("error/reason", "Sorry, could not map error code '" +
					errorCode + "'. Please report this incident at https://issues.libelektra.org/");
			return new InternalException(temporaryError);
		}

	}

}
