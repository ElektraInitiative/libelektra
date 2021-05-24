package org.libelektra.exception;

import static org.libelektra.exception.KDBException.META_KEY_NOT_FOUND_VALUE;

import org.libelektra.Key;

public class ExceptionMapperService
{

	/**
	 * This method extracts error and warning information and returns is as an
	 * exception
	 *
	 * @param k The key containing error/* and warnings/* metakeys
	 * @return The corresponding mapped exception
	 */
	public static KDBException getMappedException (Key k)
	{
		String errorNumber = k.getMeta ("error/number").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);

		if (errorNumber.equals (ResourceException.errorNumber ()))
		{
			return new ResourceException (k);
		}
		if (errorNumber.equals (OutOfMemoryException.errorNumber ()))
		{
			return new OutOfMemoryException (k);
		}
		if (errorNumber.equals (InternalException.errorNumber ()))
		{
			return new InternalException (k);
		}
		if (errorNumber.equals (InterfaceException.errorNumber ()))
		{
			return new InterfaceException (k);
		}
		if (errorNumber.equals (InstallationException.errorNumber ()))
		{
			return new InstallationException (k);
		}
		if (errorNumber.equals (PluginMisbehaviorException.errorNumber ()))
		{
			return new PluginMisbehaviorException (k);
		}
		if (errorNumber.equals (ConflictingStateException.errorNumber ()))
		{
			return new ConflictingStateException (k);
		}
		if (errorNumber.equals (SyntacticValidationException.errorNumber ()))
		{
			return new SyntacticValidationException (k);
		}
		if (errorNumber.equals (SemanticValidationException.errorNumber ()))
		{
			return new SemanticValidationException (k);
		}

		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", InternalException.errorNumber ());
		temporaryError.setMeta ("error/reason", "Sorry, could not map error number '" + errorNumber +
								"'. Please report this incident at https://issues.libelektra.org/");
		return new InternalException (temporaryError);
	}
}
