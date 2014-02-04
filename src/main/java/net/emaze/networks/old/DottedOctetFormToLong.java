package net.emaze.networks.old;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;

public class DottedOctetFormToLong implements Delegate<Long, String> {

    @Override
    public Long perform(String dottedOctectForm) {
        dbc.precondition(dottedOctectForm != null, "dottedOctectForm cannot be null");
        final String[] split = dottedOctectForm.split("\\.");
        dbc.precondition(split.length == 4, "dottedOctetForm is not valid");
        final long firstOctet = Long.decode(split[0]);
        final long secondOctet = Long.decode(split[1]);
        final long thirdOctet = Long.decode(split[2]);
        final long fourthOctet = Long.decode(split[3]);
        dbc.precondition(isValidOctet(firstOctet) && isValidOctet(secondOctet) && isValidOctet(thirdOctet) && isValidOctet(fourthOctet), "dottedOctetForm is not valid");
        return ((firstOctet << 24) | (secondOctet << 16) | (thirdOctet << 8) | fourthOctet) & 0xFFFFFFFFL;
    }

    private boolean isValidOctet(long octet) {
        return (octet >= 0) && (octet < 256);
    }
}
