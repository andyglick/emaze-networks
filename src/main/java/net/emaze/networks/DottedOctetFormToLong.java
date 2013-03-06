package net.emaze.networks;

import java.util.regex.Pattern;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;

public class DottedOctetFormToLong implements Delegate<Long, String> {

    public static final Pattern DOTTED_OCTET_FORM = Pattern.compile("^([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\.([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\.([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\.([01]?\\d\\d?|2[0-4]\\d|25[0-5])$");

    @Override
    public Long perform(String dottedOctectForm) {
        dbc.precondition(dottedOctectForm != null, "dottedOctectForm cannot be null");
        dbc.precondition(DOTTED_OCTET_FORM.matcher(dottedOctectForm).matches(), "dottedOctectForm is not valid.");
        final String[] split = dottedOctectForm.split("\\.");
        final long firstOctet = Long.parseLong(split[0]);
        final long secondOctet = Long.parseLong(split[1]);
        final long thirdOctet = Long.parseLong(split[2]);
        final long fourthOctet = Long.parseLong(split[3]);
        return ((firstOctet << 24) | (secondOctet << 16) | (thirdOctet << 8) | fourthOctet) & 0xFFFFFFFFL;
    }
}
