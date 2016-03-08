package net.emaze.networks.ipv4;

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

public class Ipv4SubtractNetworksFromNetwork implements BiFunction<Ipv4Network, Collection<Ipv4Network>, Set<Ipv4Network>> {

    private final Function<Ipv4Network, DenseRange<Ipv4>> cidrToDenseRange = Compositions.compose(Tuples.tupled(new Ipv4ToDenseRange()), new Ipv4NetworkToIp());
    private final Function<DenseRange<Ipv4>, List<Ipv4Network>> denseRangeToCidr = Compositions.compose(Tuples.tupled(new Ipv4RangeToNetworks()), new Ipv4DenseRangeToIp());

    @Override
    public Set<Ipv4Network> apply(Ipv4Network parent, Collection<Ipv4Network> children) {
        dbc.precondition(parent != null, "parent cannot be null");
        dbc.precondition(children != null, "children cannot be null");
        final Range<Ipv4> parentAsRange = cidrToDenseRange.apply(parent);
        final Iterator<Range<Ipv4>> childrenAsRanges = Applications.transform(children, Compositions.compose(new Vary< DenseRange<Ipv4>, Range<Ipv4>>(), cidrToDenseRange));
        final Range<Ipv4> remainder = IpRanges.RANGESV4.difference(Multiplexing.chain(Iterations.iterator(parentAsRange), childrenAsRanges));
        final Iterator<DenseRange<Ipv4>> densifiedNotEmptyRemainders = Filtering.filter(remainder.densified(), Logic.not(new RangeIsEmpty<DenseRange<Ipv4>, Ipv4>()));
        return Consumers.all(Multiplexing.flatten(Applications.transform(densifiedNotEmptyRemainders, denseRangeToCidr)), new HashSet<Ipv4Network>());
    }
}
