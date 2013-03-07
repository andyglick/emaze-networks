package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;

public class LongToDottedOctetForm implements Delegate<String, Long> {

    @Override
    public String perform(Long longForm) {
        dbc.precondition(longForm != null, "longForm cannot be null");
        final long first = (longForm & 0xFF000000) >> 24;
        final long second = (longForm & 0x00FF0000) >> 16;
        final long third = (longForm & 0x0000FF00) >> 8;
        final long fourth = longForm & 0x000000FF;
        return String.format("%s.%s.%s.%s", first, second, third, fourth);
    }
}
