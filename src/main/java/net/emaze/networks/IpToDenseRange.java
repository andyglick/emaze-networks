package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.ranges.DenseRange;

public class IpToDenseRange implements BinaryDelegate<DenseRange<Ip>, Ip, Ip> {


    @Override
    public DenseRange<Ip> perform(Ip first, Ip last) {
        dbc.precondition(first != null && last != null, "boundaries must be not null");
        dbc.precondition(first.version().equals(last.version()), "addresses must be expressed using the same format");
        return (DenseRange<Ip>) first.version().getRanges().closed(first, last);
    }
}
