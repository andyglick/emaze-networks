package net.emaze.networks.ipv6;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import net.emaze.dysfunctional.Applications;
import net.emaze.dysfunctional.Compositions;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Filtering;
import net.emaze.dysfunctional.Iterations;
import net.emaze.dysfunctional.Logic;
import net.emaze.dysfunctional.Multiplexing;
import net.emaze.dysfunctional.Tuples;
import net.emaze.dysfunctional.casts.Vary;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.ranges.RangeIsEmpty;
import net.emaze.networks.IpRanges;

public class Ipv6SubtractNetworksFromNetwork implements BiFunction<Ipv6Network, Collection<Ipv6Network>, Set<Ipv6Network>> {

    private final Function<Ipv6Network, DenseRange<Ipv6>> cidrToDenseRange = Compositions.compose(Tuples.tupled(new Ipv6ToDenseRange()), new Ipv6NetworkToIp());
    private final Function<DenseRange<Ipv6>, List<Ipv6Network>> denseRangeToCidr = Compositions.compose(Tuples.tupled(new Ipv6RangeToNetworks()), new Ipv6DenseRangeToIp());

    @Override
    public Set<Ipv6Network> apply(Ipv6Network parent, Collection<Ipv6Network> children) {
        dbc.precondition(parent != null, "parent cannot be null");
        dbc.precondition(children != null, "children cannot be null");
        final Range<Ipv6> parentAsRange = cidrToDenseRange.apply(parent);
        final Iterator<Range<Ipv6>> childrenAsRanges = Applications.transform(children, Compositions.compose(new Vary<DenseRange<Ipv6>, Range<Ipv6>>(), cidrToDenseRange));
        final Range<Ipv6> remainder = IpRanges.RANGESV6.difference(Multiplexing.chain(Iterations.iterator(parentAsRange), childrenAsRanges));
        final Iterator<DenseRange<Ipv6>> densifiedNotEmptyRemainders = Filtering.filter(remainder.densified(), Logic.not(new RangeIsEmpty<DenseRange<Ipv6>, Ipv6>()));
        return Consumers.all(Multiplexing.flatten(Applications.transform(densifiedNotEmptyRemainders, denseRangeToCidr)), new HashSet<Ipv6Network>());
    }
}
