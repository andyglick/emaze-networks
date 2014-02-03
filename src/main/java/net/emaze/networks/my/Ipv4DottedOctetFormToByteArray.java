package net.emaze.networks.my;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;

public class Ipv4DottedOctetFormToByteArray implements Delegate<byte[], String> {

    @Override
    public byte[] perform(String dottedOctetForm) {
        dbc.precondition(dottedOctetForm != null, "dottedOctetForm cannot be null");
        final String[] split = dottedOctetForm.split("\\.");
        dbc.precondition(split.length == 4, "dottedOctetForm is not valid");
        return new byte[]{decode(split[0]), decode(split[1]), decode(split[2]), decode(split[3])};
    }

    private byte decode(String s) {
        final Integer octet = Integer.decode(s);
        dbc.precondition((octet & 0xFFFFFF00) == 0, "dotted octet form is not valid");
        return octet.byteValue();
    }

}
