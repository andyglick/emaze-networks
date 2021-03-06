package net.emaze.networks;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.networks.ipv4.Ipv4;
import net.emaze.networks.ipv4.Ipv4SequencingPolicy;
import net.emaze.networks.ipv6.Ipv6;
import net.emaze.networks.ipv6.Ipv6SequencingPolicy;

public abstract class IpRanges {

    public static Ranges<Ipv4> RANGESV4 = new Ranges<>(new ComparableComparator<Ipv4>(), new Ipv4SequencingPolicy(), Ipv4.getFirstIp());
    public static Ranges<Ipv6> RANGESV6 = new Ranges<>(new ComparableComparator<Ipv6>(), new Ipv6SequencingPolicy(), Ipv6.getFirstIp());

}
