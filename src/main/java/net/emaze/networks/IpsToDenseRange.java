package net.emaze.networks;

import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.ranges.DenseRange;

public class IpsToDenseRange implements BinaryDelegate<DenseRange<Ip>, Ip, Ip> {


    @Override
    public DenseRange<Ip> perform(Ip first, Ip last) {
        return (DenseRange<Ip>) new IpRanges().closed(first, last);
    }
}
