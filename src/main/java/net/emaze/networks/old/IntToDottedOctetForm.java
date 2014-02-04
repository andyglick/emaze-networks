package net.emaze.networks.old;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;

public class IntToDottedOctetForm implements Delegate<String, Integer> {

    @Override
    public String perform(Integer bits) {
        dbc.precondition(bits != null, "longForm cannot be null");
        final int first = (bits & 0xFF000000) >>> 24;
        final int second = (bits & 0x00FF0000) >> 16;
        final int third = (bits & 0x0000FF00) >> 8;
        final int fourth = bits & 0x000000FF;
        return String.format("%s.%s.%s.%s", first, second, third, fourth);
    }
}
