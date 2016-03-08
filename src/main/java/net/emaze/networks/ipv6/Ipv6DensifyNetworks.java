package net.emaze.networks.ipv6;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import net.emaze.dysfunctional.Applications;
import net.emaze.dysfunctional.Compositions;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Multiplexing;
import net.emaze.dysfunctional.Tuples;
import net.emaze.dysfunctional.casts.Vary;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.networks.IpRanges;

public class Ipv6DensifyNetworks implements Function<Collection<Ipv6Network>, Set<Ipv6Network>> {

    private final Function<Ipv6Network, DenseRange<Ipv6>> cidrToDenseRange = Compositions.compose(Tuples.tupled(new Ipv6ToDenseRange()), new Ipv6NetworkToIp());
    private final Function<DenseRange<Ipv6>, List<Ipv6Network>> denseRangeToCidr = Compositions.compose(Tuples.tupled(new Ipv6RangeToNetworks()), new Ipv6DenseRangeToIp());

    @Override
    public Set<Ipv6Network> apply(Collection<Ipv6Network> cidrs) {
        dbc.precondition(cidrs != null, "cidrs cannot be null");
        final Iterator<Range<Ipv6>> childrenAsRanges = Applications.transform(cidrs, Compositions.compose(new Vary<DenseRange<Ipv6>, Range<Ipv6>>(), cidrToDenseRange));
        final Range<Ipv6> union = IpRanges.RANGESV6.union(childrenAsRanges);
        final List<DenseRange<Ipv6>> densifiedUnion = union.densified();
        return Consumers.all(Multiplexing.flatten(Applications.transform(densifiedUnion, denseRangeToCidr)), new HashSet<Ipv6Network>());
    }

}
