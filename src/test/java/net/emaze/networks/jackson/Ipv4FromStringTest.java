package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import net.emaze.networks.ipv4.Ipv4;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4FromStringTest {

    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'ip':'127.0.0.1'}".replace("'", "\"");
        final Ipv4 expected = Ipv4.parse("127.0.0.1");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv4 got = mapper.readValue(serialized, BeanWithIpv4.class);
        Assert.assertEquals(expected, got.getIp());
    }

}
