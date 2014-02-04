package net.emaze.networks.old;

import java.util.Iterator;
import java.util.List;
import net.emaze.dysfunctional.Applications;
import net.emaze.dysfunctional.Strings;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.options.Box;
import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv6FormatToCompressedForm implements Delegate<String, String> {

    @Override
    public String perform(String ip) {
        final String[] pieces = ip.split(":");
        final List<String> shorter = Applications.map(pieces, new RemoveHeadingZeroes());
        final Maybe<Pair<Integer, Integer>> maxIndex = findBlasdi(shorter.iterator());
        if (maxIndex.hasValue()) {
            final List<String> former = shorter.subList(0, maxIndex.value().first());
            final List<String> latter = shorter.subList(maxIndex.value().first() + maxIndex.value().second(), shorter.size());
            return Strings.interpose(former, ":") + "::" + Strings.interpose(latter, ":");
        }
        return Strings.interpose(shorter, ":");
    }

    private Maybe<Pair<Integer, Integer>> findBlasdi(Iterator<String> pieces) {
        int max = 0;
        final Box<Integer> maxIndex = Box.empty();
        int currentSequenceIndex = 0;
        int currentSequenceLength = 0;
        while (pieces.hasNext()) {
            final String piece = pieces.next();
            if (piece.equals("0")) {
                currentSequenceLength++;
                if (currentSequenceLength >= 2 && currentSequenceLength > max) {
                    max = currentSequenceLength;
                    maxIndex.setContent(currentSequenceIndex);
                }
            } else {
                currentSequenceIndex += currentSequenceLength + 1;
                currentSequenceLength = 0;
            }
        }
        final int maxLength = max;
        return maxIndex.unload().fmap(new Delegate<Pair<Integer, Integer>, Integer>() {
            @Override
            public Pair<Integer, Integer> perform(Integer maxIndex) {
                return Pair.of(maxIndex, maxLength);
            }
        });
    }

    private static class RemoveHeadingZeroes implements Delegate<String, String> {

        @Override
        public String perform(String piece) {
            final int length = piece.length() - 1;
            for (int i = 0; i < length; i++) {
                if (piece.startsWith("0")) {
                    piece = piece.substring(1);
                }
            }
            return piece;
        }
    }
}
