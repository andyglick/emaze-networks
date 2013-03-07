package net.emaze.networks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.order.CompareToBuilder;
import net.emaze.dysfunctional.order.Order;
import net.emaze.dysfunctional.ranges.DenseRange;

public class IpRangeToCidrs implements Delegate<List<Cidr>, DenseRange<Ipv4>> {

    @Override
    public List<Cidr> perform(DenseRange<Ipv4> range) {
        dbc.precondition(range != null, "range cannot be null");
        dbc.precondition(range.end().hasValue(), "range cannot be open-ended");

        // An empty range results in an empty list of CIDRs
        if (!range.iterator().hasNext()) {
            return Collections.emptyList();
        }

        // Obtain a spanning cidr containing entire range
        final Cidr spanningCidr = new IpRangeToSpanningCidr().perform(range);
        final Ipv4 startIp = range.begin();
        // FIXME FIXME FIXME: A closed range SHOULD NOT return as last ip the first ip outside range !!!!!!!!!!
        final Ipv4 endIp = range.end().value().previous();

        if (spanningCidr.first().equals(startIp) && spanningCidr.last().equals(endIp)) {
            // Spanning CIDR matches start and end exactly;
            return Collections.singletonList(spanningCidr);
        }
        if (spanningCidr.last().equals(endIp)) {
            // Spanning CIDR matches range end exactly;
            final Ipv4 previousIp = startIp.previous();
            final List<Cidr> result = new ArrayList<>();
            final List<Cidr> remainder = new SubtractIpFromCidr().perform(spanningCidr, previousIp);
            boolean firstFound = false;
            for (Cidr cidr : remainder) {
                if (cidr.first().equals(startIp)) {
                    firstFound = true;
                }
                if (firstFound) {
                    result.add(cidr);
                }
            }
            return result;
        }
        if (spanningCidr.first().equals(startIp)) {
            // Spanning CIDR matches range start exactly
            final Ipv4 nextIp = endIp.next();
            final List<Cidr> result = new ArrayList<>();
            final List<Cidr> remainder = new SubtractIpFromCidr().perform(spanningCidr, nextIp);
            for (Cidr cidr : remainder) {
                result.add(cidr);
                if (cidr.last().equals(endIp)) {
                    break;
                }
            }
            return result;
        }
        if (Order.of(spanningCidr.first().compareTo(startIp)) != Order.GT && Order.of(spanningCidr.last().compareTo(endIp)) != Order.LT) {
            // Spanning CIDR overlaps entire range
            final Ipv4 previousIp = startIp.previous();
            final LinkedList<Cidr> result = new LinkedList<>();
            final List<Cidr> remainder = new SubtractIpFromCidr().perform(spanningCidr, previousIp);
            boolean firstFound = false;
            for (Cidr cidr : remainder) {
                if (cidr.first().equals(startIp)) {
                    firstFound = true;
                }
                if (firstFound) {
                    result.add(cidr);
                }
            }
            final Ipv4 nextIp = endIp.next();
            final List<Cidr> remainder2 = new SubtractIpFromCidr().perform(result.removeLast(), nextIp);
            for (Cidr cidr : remainder2) {
                result.add(cidr);
                if (cidr.last().equals(endIp)) {
                    break;
                }
            }
            return result;
        }
        return Collections.emptyList();
    }
}
