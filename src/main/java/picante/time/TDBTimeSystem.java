package picante.time;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;

class TDBTimeSystem implements TimeSystem<Double> {

  public TSEpoch getTSEpoch(double time) {
    return new TSEpoch(TSEpoch.splitDouble(time));

  }

  @Override
  public TSEpoch getTSEpoch(Double time) {
    return getTSEpoch(time.doubleValue());
  }

  @Override
  public Double getTime(TSEpoch tse) {
    return tse.getSeconds() + tse.getFractionalSecs();
  }

  @Override
  public Double add(Double t0, double deltaT) {
    return t0 + deltaT;
  }



  @Override
  public double difference(Double t0, Double t1) {
    return t0 - t1;
  }

  @Override
  public double difference(TSEpoch t0, TSEpoch t1) {
    return getTime(t0) - getTime(t1);
  }

  @Override
  public TSEpoch add(TSEpoch t0, double deltaT) {
    double et0 = getTime(t0);
    Double et1 = add(et0, deltaT);
    return getTSEpoch(et1);

  }

  public static void main(String[] args) {
    final TDBTimeSystem ets = new TDBTimeSystem();
    JFrame top = new JFrame("ET Test");
    top.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    top.setLayout(new GridBagLayout());
    JLabel inputLabel = new JLabel("Input seconds: ");
    final JTextField inputField = new JTextField();
    inputField.setPreferredSize(new Dimension(600, 30));
    final JLabel outputLabel = new JLabel("");
    outputLabel.setPreferredSize(new Dimension(600, 30));
    outputLabel.setBackground(Color.WHITE);
    outputLabel.setBorder(BorderFactory.createLoweredBevelBorder());
    JButton goButton = new JButton("Ingest");
    goButton.addActionListener(new ActionListener() {

      @Override
      public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
        double time = Double.parseDouble(inputField.getText());
        TSEpoch tse = ets.getTSEpoch(time);
        outputLabel.setText(tse.toString());
      }
    });

    GridBagConstraints c = new GridBagConstraints();
    c.gridx = 0;
    c.gridy = 0;
    top.add(inputLabel, c);
    c.gridx = 1;
    top.add(inputField, c);
    c.gridx = 0;
    c.gridy = 1;
    top.add(goButton, c);
    c.gridx = 1;
    top.add(outputLabel, c);

    top.pack();
    top.setVisible(true);

  }



}
