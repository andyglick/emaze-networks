package net.emaze.networks;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.CompareToBuilder;
import net.emaze.dysfunctional.tuples.Pair;

public class SubtractIpFromCidr implements BinaryDelegate<List<Cidr>, Cidr, Ipv4> {

    @Override
    public List<Cidr> perform(Cidr minuend, Ipv4 subtrahend) {
        final List<Cidr> reminder = recursivelySubtract(minuend, subtrahend);
        Collections.sort(reminder, new FirstIpThenLastIpCidrComparator());
        return reminder;
    }

    private List<Cidr> recursivelySubtract(Cidr minuend, Ipv4 subtrahend) {
        if (!minuend.contains(subtrahend)) {
            // This CIDR does not contains minuend, so return it without changes
            return Arrays.asList(minuend);
        }
        if (minuend.netmask().isNarrowest()) {
            // This CIDR contains only the minuend, so return an empty list
            return Arrays.asList();
        }
        final List<Cidr> reminder = new LinkedList<>();
        final Pair<Cidr, Cidr> split = minuend.split();
        if (split.first().contains(subtrahend)) {
            reminder.add(split.second());
            reminder.addAll(recursivelySubtract(split.first(), subtrahend));
        } else {
            reminder.add(split.first());
            reminder.addAll(recursivelySubtract(split.second(), subtrahend));
        }
        return reminder;
    }

    private static class FirstIpThenLastIpCidrComparator implements Comparator<Cidr> {

        @Override
        public int compare(Cidr lhs, Cidr rhs) {
            return new CompareToBuilder().append(lhs.first(), rhs.first()).append(lhs.last(), rhs.last()).toComparison();
        }
    }
}
